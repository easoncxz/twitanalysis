module TwitAnalysis.Utils where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Monoid (Monoid, mconcat)

mintercalate :: (Monoid s) => s -> [s] -> s
mintercalate sep list = mconcat (List.intersperse sep list)

elseDo :: (MonadIO m) => Maybe a -> m a -> m a
elseDo def action =
  case def of
    Nothing -> action
    Just v -> return v
