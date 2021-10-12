module Util where

import           Reflex.Dom                     ( EventWriterT
                                                , Reflex(Event)
                                                , runEventWriterT
                                                )
import Data.Semigroup (First(First, getFirst))

execEventWriterT
    :: (Reflex t, Semigroup w, Monad f) => EventWriterT t w f a -> f (Event t w)
execEventWriterT = fmap snd . runEventWriterT

execFirstEventWriterT
  :: (Monad m, Reflex t) => EventWriterT t (First w) m a -> m (Event t w)
execFirstEventWriterT = fmap (fmap getFirst) . execEventWriterT
