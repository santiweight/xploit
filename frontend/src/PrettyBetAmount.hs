{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrettyBetAmount where

import           BasicPrelude
import           Data.Text                      ( unpack )
import Poker.Query.ActionIx
import Poker
import Money

class PrettyBetAmount b where
  prettyBetAmount :: b -> Text

instance PrettyBetAmount (IxRange (Amount "USD")) where
  prettyBetAmount = (<> "$") . \case
    ExactlyRn amt -> pretty amt
    BetweenRn lo hi -> "[" <> pretty lo <> "," <> pretty hi <> "]"
    AnyRn -> "any amount"
    AboveRn amt -> "above " <> tshow amt
    BelowRn amt -> "below " <> tshow amt
    where pretty amt = tshow amt

instance PrettyBetAmount (Amount "USD") where
  -- TODO double check approx
  prettyBetAmount (Amount amt) = discreteToDecimal defaultDecimalConf Round amt

deriving instance PrettyBetAmount b => PrettyBetAmount (Stack b)