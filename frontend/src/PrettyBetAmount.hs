{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyBetAmount where

import           BasicPrelude
import           Data.Text                      ( unpack )
import           Poker.Base

class PrettyBetAmount b where
  prettyBetAmount :: b -> Text

instance PrettyBetAmount (IxRange BetSize) where
  prettyBetAmount = (<> "$") . \case
    ExactlyRn amt -> pretty amt
    BetweenRn lo hi -> "[" <> pretty lo <> "," <> pretty hi <> "]"
    amt -> error . unpack $ "pretty not yet implemented: " <> tshow amt
    where pretty (BetSize amt) = tshow amt