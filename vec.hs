module Vec where 

import Control.Lens

data Vec2 a = Vec2 a a
    deriving (Eq, Show)
type Vec2f = Vec2 Float

instance Functor Vec2 where
    fmap f (Vec2 x y) = Vec2 (f x) (f y)

raiseOp op (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (op x1 x2) (op y1 y2)
instance (Num a) => Num (Vec2 a) where
    (+) = raiseOp (+)
    (-) = raiseOp (-)
    (*) = raiseOp (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger i = Vec2 (fromInteger i) (fromInteger i)

vec2 (x, y) = Vec2 x y

unitVec t = Vec2 (cos t) (sin t)

magnitude2 (Vec2 x y) = x^2 + y^2
magnitude = sqrt . magnitude2
normalize v = v /. magnitude v

s .* v = fmap (* s) v
v *. s = fmap (* s) v
v /. s = fmap (/ s) v

minVec :: (Ord a) => Vec2 a -> Vec2 a -> Vec2 a
minVec = raiseOp min

maxVec :: (Ord a) => Vec2 a -> Vec2 a -> Vec2 a
maxVec = raiseOp max

clampVec :: (Ord a) => Vec2 a -> Vec2 a -> Vec2 a -> Vec2 a
clampVec lo hi = minVec hi . maxVec lo

xLens :: Lens' Vec2f Float
xLens = lens getX setX
getX (Vec2 x _) = x
setX (Vec2 _ y) x = Vec2 x y

yLens :: Lens' Vec2f Float
yLens = lens getY setY
getY (Vec2 _ y) = y
setY (Vec2 x _) y = Vec2 x y
