import Numeric.LinearAlgebra
import Prelude hiding ((<>))

type Vec = Vector R
type Mat = Matrix R

reg1dim1 :: Vec -> Vec -> R
reg1dim1 x y = x <.> y / x <.> x

main = print $ reg1dim1 x y
    where
        x = vector [1, 2, 4, 6, 7]
        y = vector [1, 3, 3, 5, 4]
