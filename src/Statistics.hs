module Statistics
    ( mean
    , equilibrate
    -- , meanSquare
    ) where

import qualified Data.Vector   as V
import Data.List (genericLength)
-- import           Data.Sequence


equilibrate :: (Real a) => Int -> [a] -> [a]
equilibrate eq xs = drop (quot (length xs) eq) xs 

mean :: (Real a) => [a] -> Rational
mean xs = toRational (sum xs) / toRational(genericLength xs)

