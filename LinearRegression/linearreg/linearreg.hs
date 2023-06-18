{-
    BUG:
    when adding bias to y, it outputs w = [1.0 2.1 3.0].
-}

import Numeric.LinearAlgebra
import System.Random
import Prelude hiding ((<>))

type Vec = Vector R
type Mat = Matrix R

fit :: Mat -> Vec -> Vec
fit x_til t = inv((tr x_til) <> x_til) #> ((tr x_til) #> t)

predict :: Vec -> Vec -> R
predict w x = w <.> (vector $ [1.0] ++ toList x)

addBias :: Mat -> Mat
addBias x = tr $ matrix (rows x) new_list
    where new_list = (take (rows x) [1.0,1.0..]) ++ (toList $ flatten x)

n       = 100
_scale  = 10

w0 = 1.0
w1 = 2.0
w2 = 3.0

main = do
    print w
    print $ predict w $ vector [1, 1]
        where
            gen     = mkStdGen 7
            x       = matrix 2 $ take (n * 2) $ randomRs (0, _scale) gen
            x_til   = addBias x
            y       = (x_til #> vector [w0, w1, w2]) -- + dy
            dy      = vector $ take n $ randomRs (-1.0, 1.0) gen
            w       = fit x_til y
