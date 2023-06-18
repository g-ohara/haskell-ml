import Numeric.LinearAlgebra
import Prelude hiding ((<>))

type Vec = Vector R
type Mat = Matrix R

reg1dim2 :: Vec -> Vec -> Vec
reg1dim2 x y = vector [a, b]
    where
        x_sum   = sumElements x
        y_sum   = sumElements y
        n       = fromIntegral $ size x
        a       = (x <.> y - x_sum * y_sum / n) / (x <.> x - x_sum ** 2 / n)
        b       = (y_sum - a * x_sum) / n

main = print $ reg1dim2 x y
    where
        x = vector [1, 2, 4, 6, 7]
        y = vector [1, 3, 3, 5, 4]
