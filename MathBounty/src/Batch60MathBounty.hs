{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module Batch60MathBounty where

import           Control.Monad             (void)
import Data.Default               (Default (..))
import Data.Text                  (Text)
import Data.Void
import Data.Map as Map
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import           Plutus.Contract
import           Plutus.Trace.Emulator  as Emulator
import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts  --Ledger.Script.Utils.V1.Scripts or for V2 Plutus.Script.Utils.V2.Scripts
import           Ledger.Ada                as Ada
import           Playground.Contract
import qualified Prelude
import Prelude (IO, Show, String, show, Semigroup (..))
import           Text.Printf          (printf)
import Ledger.TimeSlot
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import PlutusTx.IsData.Class (toBuiltinData)


--ON-LINE
newtype MathBountyDatum = MBD 
                     { mbMath :: Integer }
 
--PlutusTx.unstableMakeIsData ''MathBountyDatum 
PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)]


{-# INLINABLE mathBountyValidator #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool
mathBountyValidator datum guess sContext = traceIfFalse "Wrong guess!" ((mbMath datum) == guess*guess)


data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty  
bountyValidator = Scripts.mkTypedValidator @MathBounty
                $$(PlutusTx.compile [|| mathBountyValidator ||])
                $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MathBountyDatum @Integer  --mkUntypedValidator mathBountyValidator

--TypedValidator {tvValidator = Validator { <script> }, tvValidatorHash = 8fec4d22d825020eafc67a3545cee247b466e88d5bed14e5bf0cee3f, tvForwardingMPS = MintingPolicy { <script> }, tvForwardingMPSHash = 7de420c052239ed413d77433673704fea53a102397ae7e085a54e998}


validator :: Validator  -- Validator {<scripts>}
validator = Scripts.validatorScript bountyValidator

bountyAddress :: Ledger.Address
bountyAddress = scriptAddress validator  


--OFF-CHAIN

data BountyParams = BP
                  { bpMath :: Integer
                  , bpAmount :: Integer
                  } deriving (Generic, FromJSON, ToJSON, ToSchema)

type MathBountySchema =
        Endpoint "bounty" BountyParams
    .\/ Endpoint "solution" Integer


bounty :: BountyParams -> Contract () MathBountySchema Text ()
bounty (BP math amount) = do
                         let datum = MBD 
                                   { mbMath = math}
                             tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)
                         ledgerTx <- submitTxConstraints bountyValidator tx
                         void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                         logInfo @String $ printf "Bounty created of amount = %s" (show amount)

solution :: Integer -> Contract () MathBountySchema Text ()
solution guess = do
                 utxos <- utxosAt bountyAddress
                 logInfo @String $ printf "The utxos: %s " (show $ Map.toList utxos)
                 case Map.toList utxos of
                    []              -> logInfo @String $ printf "No UTxOs on the Contract!"
                    (oref,a):xs     -> do
                                       let lookups  = Constraints.otherScript validator <>
                                                      Constraints.unspentOutputs (Map.fromList [(oref,a)]) 
                                           tx       = Constraints.mustSpendScriptOutput oref (Redeemer $ toBuiltinData guess)
                                       ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                       void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                       logInfo @String $ printf "Proposed solution was: %s " (show guess)

endpoints :: Contract () MathBountySchema Text ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where 
    bounty'   = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

mkSchemaDefinitions ''MathBountySchema
mkKnownCurrencies []