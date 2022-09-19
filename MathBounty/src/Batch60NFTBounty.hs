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

module Batch60NFTBounty where

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
import qualified Ledger.Typed.Scripts      as Scripts  --Ledger.V1.Utils Ledger.V3.Utils
import           Ledger.Ada                as Ada
import           Ledger.Value              as Value
import           Playground.Contract
import qualified Prelude
import Prelude (IO, Show, String, show, Semigroup (..))
import           Text.Printf          (printf)
import Ledger.TimeSlot
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import PlutusTx.IsData.Class (toBuiltinData)

