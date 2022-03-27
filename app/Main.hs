{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
    
import CSVParser
import Helper
import Text.CSV
import Data.MultiSet (fromList, toOccurList)

p = putStrLn . either show show

main :: IO ()
main = do
  test_csv <- parseCSVFromFile "salaries.csv"
  case test_csv of
    Right csv -> updateValues csv
    Left err -> print err

updateValues csv = do
    let max = applyToColumnInCSV (maximum . readColumn) csv "age"
    p max
    let min = applyToColumnInCSV (minimum . readColumn) csv "age"
    p min
    let avg = applyToColumnInCSV (average . readColumn) csv "age"
    p avg






