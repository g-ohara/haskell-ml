import NeuralNetwork
import Numeric.LinearAlgebra

main :: IO ()
main = do
    cs1 <- readFile "dataset/train_data.dat"
    cs2 <- readFile "dataset/test_data.dat"
    cs3 <- readFile "dataset/train_label.dat"
    cs4 <- readFile "dataset/test_label.dat"

    let batchSize  = 100

    let trainDataList   = map read $ lines cs1
    let testDataList    = map read $ lines cs2
    let trainLabelList  = map read $ lines cs3
    let testLabelList   = map read $ lines cs4

    weight <- flatten <$> randn 1 weight_size

    let trainData   = matrix inputSize $ take (batchSize * inputSize) trainDataList
    let testData    = matrix inputSize testDataList
    let trainLabel  = oneHotMat outputSize $ take batchSize trainLabelList
    let testLabel   = oneHotMat outputSize testLabelList

    putStr "Now Loading Training Data...\n"
    putStr "size of train data  : "
    print $ size trainData
    putStr "size of train label : "
    print $ size trainLabel
    putStr "size of test data   : "
    print $ size testData
    putStr "size of test label  : "
    print $ size testLabel

    -- putStr "Gradient Check      : "
    -- print $ gradientCheck weight x t

    let learningRate = 0.1
    let iterNum = 100
    let newW = learn weight trainData trainLabel learningRate iterNum

    print $ testAccuracy newW trainData trainLabel
    print $ testAccuracy newW testData testLabel

