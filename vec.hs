module Vec where 

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

s .*/ v = fmap (* s) v
v /*. s = fmap (* s) v

