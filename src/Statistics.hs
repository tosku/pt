module Statistics
    ( mean
    ) where

import Data.List (genericLength)


mean :: (Real a) => [a] -> Rational
mean xs = toRational (sum xs) / toRational(genericLength xs)

