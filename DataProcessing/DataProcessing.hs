module DataProcessing where

import Data.List
import Text.ParserCombinators.Parsec
import Data.CSV

type Feature    = [Double]
type Label      = Int
type DataSet    = [DataPoint]

data DataPoint = DataPoint {
    dFeature :: Feature,
    dLabel   :: Label
} deriving Show

strLabelToIntLabel :: [String] -> [Int]
strLabelToIntLabel strLabels = map (maybeToInt . labelToIndex) strLabels
    where
        labelToIndex l = findIndex (l ==) $ nub strLabels

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing = 0
maybeToInt (Just a) = a
        
processData :: [[String]] -> [DataPoint]
processData rawData = concatDataPoint feature labs
    where
        feature = map ((map (read :: String -> Double)) . init) $ rawData
        labs    = strLabelToIntLabel $ last $ transpose rawData

concatDataPoint :: [[Double]] -> [Int] -> [DataPoint]
concatDataPoint (f:fs) (l:ls) = DataPoint f l : concatDataPoint fs ls
concatDataPoint [] _ = []
concatDataPoint _ [] = []

readDataFromCSV :: String -> IO DataSet
readDataFromCSV fileName = do
    rawData <- parseFromFile csvFile fileName
    return $ either (\_ -> []) processData rawData 
