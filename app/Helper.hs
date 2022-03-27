module Helper where

import Data.List
import Text.CSV

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs
