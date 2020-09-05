module Utils where

import qualified Data.List as List
import Data.Monoid (Monoid, mconcat)

mintercalate :: (Monoid s) => s -> [s] -> s
mintercalate sep list = mconcat (List.intersperse sep list)
