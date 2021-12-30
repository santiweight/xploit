{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module GameLogic where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Poker
import           Poker.Game.Types
import           Poker.Query.ActionIx
import Poker.Game.Emulate (emulateAction)
import Prettyprinter
import Debug.Trace
-- import           Poker.Game.HasAvailableActions ( HasAvailableActions(..) )

-- initState :: GameState (Amount "USD")
-- initState =
--   let blindActions =
--         [ MkPostAction $ PostAction SB $ Post (Amount 12)
--         , MkPostAction $ PostAction BB $ Post (Amount 25)
--         ]
--   in  case execStateT (sequence_ $ emulateAction <$> blindActions) defState of
--         Left  e -> error $ show e
--         Right r -> r

-- defState :: GameState (Amount "USD")
-- defState = GameState
--     {
--     _stateStakes         = Stake 0
--     , _posToPlayer           = Map.fromList
--                                  [ (UTG, Player (parsePretty "AdAh") _)
--                                  , (UTG1, mkTestPlayer UTG1 "Ac4c" 2 20)
--                                  , (UTG2, mkTestPlayer UTG2 "KdTh" 3 15)
--                                  , (BU, mkTestPlayer BU "7h7d" 4 10)
--                                  , (SB, mkTestPlayer SB "6sTc" 5 25)
--                                  , (BB, mkTestPlayer BB "Jh5s" 6 25)
--                                  ]
--     , _potSize             = PotSize 0
--     , _street          = PreFlopBoard InitialTable
--     , _toActQueue          = [UTG, UTG1, UTG2, BU, SB, BB]
--     , _activeBet           = Nothing
--     , _streetInvestments   = Map.empty
--     , _aggressor           = Nothing
--     }
-- -- GameState
-- --   { _potSize           :: Pot g
-- --   , _street            :: Board
-- --   , _stateStakes       :: Stake g
-- --   , _aggressor         :: Maybe Position
-- --   , _toActQueue        :: [Position]
-- --   , _posToPlayer       :: Map Position (Player g)
-- --   , _streetInvestments :: Map Position g
-- --   , _activeBet         :: Maybe (ActionFaced g)
-- --   }


initRangeState :: GameState (IxRange (Amount "USD"))
initRangeState =
  let blindActions = MkPostAction <$>
        [ PostAction SB $ Post $ ExactlyRn (unsafeMkAmount 12)
        , PostAction BB $ Post $ ExactlyRn (unsafeMkAmount 25)
        ]
  in  case
          execStateT (sequence_ $ emulateAction <$> blindActions) defRangeState
        of
          Left  e -> error $ show e
          Right r -> r

defRangeState :: GameState (IxRange (Amount "USD"))
defRangeState = GameState
  { _stateStakes       = Stake AnyRn
  , _posToStack       = Map.fromList
                           [ (UTG, Stack AnyRn)
                           , (UTG1, Stack AnyRn)
                           , (UTG2, Stack AnyRn)
                           , (BU , Stack AnyRn)
                           , (SB , Stack AnyRn)
                           , (BB , Stack AnyRn)
                           ]
  , _potSize           = Pot AnyRn
  , _street            = PreFlopBoard InitialTable
  , _toActQueue        = [UTG, UTG1, UTG2, BU, SB, BB]
  , _activeBet         = Nothing
  , _streetInvestments = Map.empty
  }

-- TODO move into base packages to prevent orphan instance
-- instance HasAvailableActions (IxRange BetSize) where
--   getAvailableActions :: (MonadState (GameState (IxRange BetSize)) m) => m (Maybe (Position, [BetAction (IxRange BetSize)]))
-- getAvailableActions = use toActQueue >>= \case
--   []             -> pure Nothing
--   (toActPos : _) -> do
--     -- seatStack :: IxRange BetSize <- getSeatStack =<< getSeat toActPos undefined
--     -- streetInv :: IxRange BetSize <- use $ streetInvestments . at toActPos . non 0
--     -- stake :: BetSize             <- getStake <$> use stateStakes
--     -- callMay <- preuse (activeBet . _Just . amountFaced) >>= \case
--     --               Nothing -> Nothing
--     --               Just r -> Just r
--     callMay :: Maybe (IxRange (Amount "USD")) <- preuse
--       (activeBet . _Just . amountFaced)
--     pure $ Just
--       ( toActPos
--       , [Check, Fold, Raise 0 (BetweenRn 0.5 25), AllIn AnyRn]
--         ++ maybe [] ((: []) . Call) callMay
--       )
--     --   Nothing -> pure $ Justkj
--     --     ( toActPos
--     --     , [ Check
--     --       , AllIn $ seatStack
--     --       , Bet ((seatStack <&> (subtract (1 / 100))))
--     --       ]
--     --     ) -- [AllIn seatStack, Bet (getPossibleBet seatStack), Check, CheckTimeOut]
--     --   Just ((subtract streetInv) -> amountFaced) ->
--     --     pure . Just . (toActPos, ) $ if amountFaced == 0
--     --       then
--     --         [ Check
--     --         , Raise 0 seatStack -- TODO get the right opening bet(BetweenRn (stake * 2) seatStack)
--     --         ]
--     --       else if seatStack `leq` amountFaced
--     --         then [AllIn $ seatStack, Fold]
--     --         -- EQ -> [AllIn $ seatStack, Fold]
--     --         else if seatStack `leq` (amountFaced <&> (* 2))
--     --           then
--     --             [ AllInRaise 0 (seatStack + streetInv)
--     --             , Call amountFaced
--     --             , Fold
--     --             ]
--     --           else
--     --             [ Raise (ExactlyRn 0) (raiseMin amountFaced seatStack)
--     --             , AllInRaise (ExactlyRn 0) (seatStack + streetInv)
--     --             , Call amountFaced
--     --             , Fold
--     --             ]
--  where
--     -- raiseMin amountFaced stack = case getMinBet amountFaced of
--     --     Nothing            -> AnyRn
--     --     Just ((*2) -> amt) -> case stack of
--     --         AnyRn         -> AboveRn amt
--     --         BetweenRn _ _ -> AboveRn amt
--     --         AboveRn   _   -> AboveRn amt
--     --         BelowRn   _   -> AnyRn
--     --         ExactlyRn _   -> AboveRn amt

--     -- getMinBet AnyRn            = Nothing
--     -- getMinBet (ExactlyRn lo  ) = Just lo
--     -- getMinBet (BetweenRn lo _) = Just lo
--     -- getMinBet (AboveRn lo    ) = Just lo
--     -- getMinBet (BelowRn _     ) = Nothing

doPosAct
  ::  (Pretty b, IsBet b) => (Position, BetAction b)
  -> GameState b
  -> GameState b
doPosAct (pos, bet) gameSt =
  let emulateActionM =
        emulateAction (MkPlayerAction $ PlayerAction pos bet)
  in  case execStateT emulateActionM gameSt of
        Left  e -> traceShow e $ traceShow "BIG OLE ERROR BECAUSE INVALID ACT" $ gameSt -- TODO output error
        Right r -> r

instance (Monoid b, Semigroup b) => Semigroup (IxRange b) where
  (<>) = addRange

instance Monoid b => Monoid (IxRange b) where
  mempty = ExactlyRn mempty

instance (Ord b, IsBet b) => IsBet (IxRange b) where
  smallestAmount = ExactlyRn smallestAmount
  -- TODO fixme
  minus = subRange
