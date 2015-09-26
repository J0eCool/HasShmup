data Point = Point
    { x :: Int
    , y :: Int
    }
    deriving (Show)

flatVecOp op (Point x1 y1) (Point x2 y2) = (Point (op x1 x2) (op y1 y2))
(/+/) = flatVecOp (+)
(/-/) = flatVecOp (-)
(/*/) = flatVecOp (*)

magnitude = sqrt . fromIntegral . magnitude2
magnitude2 (Point x y) = x * x + y * y

data Shape = Rect
    { pos :: Point
    , size :: Point
    }
    | FillRect
    { pos :: Point
    , size :: Point
    }
    | Circle
    { center :: Point
    , radius :: Int
    }
    | FillCircle
    { center :: Point
    , radius :: Int
    }
    deriving (Show)

rect x y w h = Rect (Point x y) (Point w h)
fillRect x y w h = FillRect (Point x y) (Point w h)
circle x y r = Circle (Point x y) r
fillCircle x y r = FillCircle (Point x y) r

data Grid = Grid
    { width :: Int
    , height :: Int
    , shapes :: [Shape]
    }
    deriving (Show)

emptyGrid w h = Grid w h []

contains :: Shape -> Point -> Bool
contains (Rect pos size) (Point x y) =
    inRect x y left right top bot && 
    (elem x [left, right-1] || elem y [top, bot-1])
    where Point left top = pos
          Point right bot = pos /+/ size
contains (FillRect pos size) (Point x y) = inRect x y left right top bot
    where Point left top = pos
          Point right bot = pos /+/ size
contains (Circle center radius) point = any almostZero [l, r]
    where (Point dx dy) = point /-/ center
          (dx', dy') = (absMin dx dy, absMax dx dy)
          dx'' = fromIntegral dx'
          dy'' = fromIntegral dy'
          radius' = fromIntegral radius
          cx = sqrt (radius'^2 - dx''^2)
          l = cx - dy''
          r = cx + dy''
contains (FillCircle center r) point = dist <= r' - 0.5
    where diff = point /-/ center
          dist = magnitude diff
          r' = fromIntegral r

between n lo hi = n >= lo && n < hi

almostZero :: Float -> Bool
almostZero x = abs x < 0.5

absMax x y = if abs x > abs y then x else y
absMin x y = if abs x < abs y then x else y

inRect x y left right top bot = between x left right && between y top bot

isFilled :: Grid -> Point -> Bool
isFilled grid point = any pointContained shapes'
    where pointContained = contains' point
          contains' = flip contains
          shapes' = shapes grid

showGrid :: Grid -> String
showGrid grid = concat lines'
    where charAt point
            | isFilled grid point = '#'
            | otherwise = ' '
          lines' = map (\y -> map (\x -> charAt (Point x y)) [0..w] ++ "\n") [0..h]
          w = width grid - 1
          h = height grid - 1

main :: IO ()
main = do
    let grid = Grid 80 24
             [ rect 1 1 3 4
             , rect 5 5 5 5
             , rect 2 2 10 1
             , circle 16 8 4
             , circle 32 8 12
             , circle 80 50 50
             ]
    putStr $ showGrid grid
