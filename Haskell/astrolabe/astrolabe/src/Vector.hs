module Vector where

data Vec = Vec { xComp :: Double
               , yComp :: Double
               , zComp :: Double 
               } deriving(Eq, Show)

infixl 6 ^+^
infixl 6 ^-^
infixl 7 *^
infixl 7 ^*
infixl 7 ^/
infixl 7 <.>
infixl 7 ><

(^+^) :: Vec -> Vec -> Vec
Vec a b c ^+^ Vec x y z = Vec (a+x) (b+y) (c+z)

(^-^) :: Vec -> Vec -> Vec
Vec a b c ^-^ Vec x y z = Vec (a-x) (b-y) (c-z)

(*^) :: Double -> Vec -> Vec
d *^ Vec x y z = Vec (d*x) (d*y) (d*z)

(^*) :: Vec -> Double -> Vec
Vec a b c ^* d = Vec (a*d) (b*d) (c*d)

(^/) :: Vec -> Double -> Vec
Vec a b c ^/ d
    | d == 0 = error "Divide by zero"
    | otherwise = Vec (a/d) (b/d) (c/d)

(<.>) :: Vec -> Vec -> Double
Vec a b c <.> Vec x y z = (a*x) + (b*y) + (c*z)

(><) :: Vec -> Vec -> Vec
Vec a b c >< Vec x y z = Vec (b*z - c*y) (c*x - a*z) (a*y - x*b)

norm :: Vec -> Double
norm v = sqrt $ v <.> v
