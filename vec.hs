module Vec where 

data Vec2 = Vec2 Float Float
    deriving (Eq, Show)

vec2 (x, y) = Vec2 x y
--vec3 (x, y, z) = Vec3 x y z

--toV3 (Vec2 x y) = Vec3 x y 0
--toV3 v@(Vec3 _ _ _) = v

unitVec t = Vec2 (cos t) (sin t)

flatVecOp op (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (op x1 x2) (op y1 y2)
--flatVecOp op (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (op x1 x2) (op y1 y2) (op z1 z2)
--flatVecOp op v1 v2 = flatVecOp op (toV3 v1) (toV3 v2)

(/+/) = flatVecOp (+)
(/-/) = flatVecOp (-)
(/*/) = flatVecOp (*)

hoistVecOp op (Vec2 x y) = Vec2 (op x) (op y)
--hoistVecOp op (Vec3 x y z) = Vec3 (op x) (op y) (op z)
s .*/ v = hoistVecOp (* s) v
v /*. s = hoistVecOp (* s) v

