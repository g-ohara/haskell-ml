module DataSet where

data DataPoint = DataPoint {
    feature :: [Double],
    label   :: Int
} deriving Show

strLabelToIntLabel :: String -> Int
strLabelToIntLabel "Iris-setosa" = 0
strLabelToIntLabel "Iris-versicolor" = 1
strLabelToIntLabel "Iris-virginica" = 2
strLabelToIntLabel str = 3

processDataPoint :: [String] -> DataPoint
processDataPoint strs = DataPoint feature label
    where
        feature = map (read :: String -> Double) $ init strs
        label   = strLabelToIntLabel $ last strs

processData :: [[String]] -> [DataPoint]
processData rawData = map processDataPoint rawData