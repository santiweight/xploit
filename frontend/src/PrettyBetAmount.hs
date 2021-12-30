{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PrettyBetAmount where

import BasicPrelude
import Money
import Poker
import Poker.Query.ActionIx

class PrettyBetAmount b where
  prettyBetAmount :: b -> Text

instance PrettyBetAmount (IxRange (Amount "USD")) where
  prettyBetAmount =
    (<> "$") . \case
      ExactlyRn amt -> pretty amt
      BetweenRn lo hi -> "[" <> pretty lo <> "," <> pretty hi <> "]"
      AnyRn -> "any amount"
      AboveRn amt -> "above " <> tshow amt
      BelowRn amt -> "below " <> tshow amt
    where
      pretty amt = tshow amt

instance PrettyBetAmount (Amount "USD") where
  -- TODO double check approx
  prettyBetAmount (Amount amt) = discreteToDecimal defaultDecimalConf Round amt

deriving instance PrettyBetAmount b => PrettyBetAmount (Stack b)