{-# LANGUAGE DataKinds           #-} -- Hoogle each of them...
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OurGift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)

import           Plutus.Contract       

import           Plutus.Trace.Emulator as Emulator
import           Wallet.Emulator.Wallet

import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)

import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada          as Ada

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))

import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)


--import Language.Haskell.TH (RuleBndr(TypedRuleVar))

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

{-# INLINABLE typedRedeemer #-} -- Everything that its supposed to run in on-chain code need this pragma INLINABLE
typedRedeemer :: () -> Integer -> ScriptContext -> Bool   
typedRedeemer _ redeemer _ = traceIfFalse "wrong redeemer" (redeemer == 42)
 

data Typed                                            -- New type that encode the information about the Datum and the Redeemer
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()                -- Type instances to define the type of Datum
    type instance RedeemerType Typed = Integer        -- Type instance to definte the type of Redeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed      -- Tell the compiler that you are using Typed
    $$(PlutusTx.compile [|| typedRedeemer ||]) 
    $$(PlutusTx.compile [|| wrap ||])                -- Provide the translation into high level typed to low level typed
  where
    wrap = Scripts.wrapValidator @() @Integer        -- Tell wrapvalidtor which types to use for Datum and Redeemer
    
--FROM HERE ON IS TX CONSTRUCTION STUFF (REPLACEABLE BY OTHER TOOLS (CLI,KOIOS,OGMIOS,KUBER,CArdanoCL,CArdanoTxLib,Lucid....))
validator :: Validator
validator = Scripts.validatorScript typedValidator   -- Get the untyped validator script of the typeValidator PlutusCore

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator       -- Using Typed (Typed.Scripts) version of validatorHash we get the valHash of typedValidator script

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator 

--THE OFFCHAIN CODE

type GiftSchema =
            Endpoint "give" Integer  --
        .\/ Endpoint "grab" Integer

--CARDANO-CLI version of whats is happening between lines 98 and 102
--cardano-cli address build --payment-script-file vesting.plutus $TESTNET --out-file vesting.addr
--cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in abae0d0e19f75938537dc5e33252567ae3b1df1f35aafedd1402b6b9ccb7685a#0 \
    --tx-out "$(cat vesting.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file unit.json \
    --out-file tx.body
--cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed
--cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount               -- Typed version for one script, This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so unit ().
    ledgerTx <- submitTxConstraints typedValidator tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "made a gift of %d lovelace" amount                                     --This line log info,usable on the PP(Plutus Playground)

--CARDANO-CLI version of whats is happening between lines 127 and 138
--cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in 18cbe6cadecd3f89b60e08e68e5e6c7d72d730aaa1ad21431590f7e6643438ef#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral 18e93407ea137b6be63039fd3c564d4c5233e7eb7ce4ee845bc7df12c80e4df7#1 \
    --required-signer-hash c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a \
    --invalid-before 48866954 \
    --protocol-params-file protocol.json \
    --out-file tx.body
--cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed
--cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed 
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab
    --grab' = endpoint @"grab" $ const grab                                                            -- block until grab

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies [] 