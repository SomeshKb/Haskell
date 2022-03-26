{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
    
import CSVParser
import Helper

-- import qualified Data.Vector as V
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Csv
-- import GHC.Generics (Generic)

-- data Coords =
--     Coords { x :: String, y :: Int }
--     deriving (Generic)

-- instance FromNamedRecord Coords
-- instance ToNamedRecord Coords
-- instance DefaultOrdered Coords

-- -- file.csv
-- -- x,y
-- -- 1,2
-- -- 3,4

-- main :: IO ()
-- main = do
--     let list = [];
--     f <- BL.readFile "salaries.csv"
--     case decodeByName f of
--         Left err      -> print err
--         Right (_, xs) -> V.forM_ xs $ \(Coords x y) -> do
--                         let tuple1 =  (x, y)
--                         let result1  = fst tuple1
--                         print(tuple1)

import Text.CSV

main :: IO ()
main = do
  test_csv <- parseCSVFromFile "salaries.csv"
  case test_csv of
    Right csv -> updateValues csv
    Left err -> print err

updateValues csv = do
    let max = applyToColumnInCSV (maximum . readColumn) csv "Times"
    p max
    let min = applyToColumnInCSV (minimum . readColumn) csv "Times"
    p min
    let avg = applyToColumnInCSV (average . readColumn) csv "No"
    p avg

p = putStrLn . either show show


-- import Text.CSV

-- main :: IO ()
-- main = do
--     max <- applyToColumnInCSVFile (maximum . readColumn) "all_week.csv" "mag"
--     min <- applyToColumnInCSVFile (minimum . readColumn) "all_week.csv" "mag"
--     print max
--     print min
