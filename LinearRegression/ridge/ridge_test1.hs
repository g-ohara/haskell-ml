import Ridge

import Numeric.LinearAlgebra
import System.Random
import Prelude hiding ((<>))

listToString :: [R] -> String
listToString [] = ""
listToString (r:rs) = show r ++ " " ++ listToString rs

vecToString :: Vec -> String
vecToString = listToString . toList

vecsToString :: [Vec] -> String
vecsToString [] = ""
vecsToString (r:rs) = (vecToString r) ++ "\n" ++ (vecsToString rs)

matToString :: Mat -> String
matToString = vecsToString . toRows

concatMatAndVec :: Mat -> Vec -> Mat
concatMatAndVec x v = fromColumns $ toColumns x ++ [v]

n       = 100
_scale  = 10

w0 = 1.0
w1 = 2.0
w2 = 3.0

main = do
    print w
    writeFile "data/weight.dat" $ vecToString $ w
    writeFile "data/dataset.dat" $ matToString $ concatMatAndVec x y
        where
            gen     = mkStdGen 7
            x       = matrix 2 $ take (n * 2) $ randomRs (0, _scale) gen
            x_til   = addBias x
            y       = (x_til #> vector [w0, w1, w2]) + dy
            dy      = vector $ take n $ randomRs (-1.0, 1.0) gen
            w       = fit x_til y 0
