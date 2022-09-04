{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}  -- Not seen before extension (Nsb e):  
{-# LANGUAGE DeriveGeneric       #-}  -- Nseb e:
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Batch60ParametrizedContract where
    
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)       --Nsb import
import           Data.Map             as Map                  
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)


--ON-CHAIN

data VestingParams = VestingParams
                   { beneficiary   :: PaymentPubKeyHash 
                   , executionTime :: POSIXTime
                   }

PlutusTx.makeLift ''VestingParams

{-# INLINABLE parametrizedValidator #-}
parametrizedValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
parametrizedValidator params _ _ sContext = traceIfFalse "Beneficiary signature missing!" signedByBeneficiary &&
                                      traceIfFalse "Execution Time not reached!" executionTimeReached
  where
    info :: TxInfo
    info = scriptContextTxInfo sContext

    signedByBeneficiary :: Bool--           
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary params

    executionTimeReached :: Bool
    executionTimeReached = contains (from $ executionTime params) $ txInfoValidRange info

data Vesting                                                          -- Enconding the type of the datum and the redeemer 
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()                              -- Instance for the Datum
    type instance RedeemerType Vesting = ()   

paramsValidator :: VestingParams -> Scripts.TypedValidator Vesting
paramsValidator params = Scripts.mkTypedValidator @Vesting
   ($$(PlutusTx.compile [|| parametrizedValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
   $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.wrapValidator @() @()

validator :: VestingParams -> Validator
validator = Scripts.validatorScript . paramsValidator

valHash :: VestingParams -> ValidatorHash
valHash = Scripts.validatorHash . paramsValidator

scrAddress :: VestingParams -> Address
scrAddress = scriptAddress . validator

--OFF-CHAIN

type VestingSchema =
      Endpoint "give" GiveParams
  .\/ Endpoint "grab" POSIXTime

data GiveParams = GiveParams
                { gpBeneficiary     :: !PaymentPubKeyHash 
                , gpExecutionTime   :: !POSIXTime
                , gpAmount          :: !Integer
                } deriving (Generic, ToJSON, FromJSON, ToSchema)

give :: AsContractError e => GiveParams -> Contract w s e ()
give gParams = do
          let params = VestingParams
                     { beneficiary   = gpBeneficiary gParams
                     , executionTime = gpExecutionTime gParams
                     } 
              tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gParams
          ledgerTx <- submitTxConstraints (paramsValidator params) tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ printf "Made a gift of %d lovelaces to %s with execution time %s"
           (gpAmount gParams)
           (show $ gpBeneficiary gParams)
           (show $ gpExecutionTime gParams)

grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab dln = do
     now  <- currentTime
     pkh  <- ownPaymentPubKeyHash
     if now < dln
        then logInfo @String $ "Too early!"
        else do
            let params = VestingParams
                    { beneficiary   = pkh
                    , executionTime = dln
                    }
            utxos <- utxosAt $ scrAddress params
            if Map.null utxos
               then logInfo @String $ "No UTxO available!"    
               else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unsentOutputs utxos <>
                                  Constraints.otherScript (validator params)
                    tx :: TxConstraints Void Void
                    tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                               Constraints.mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "Collected gifts"          


endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema
mkKnownCurrencies []
