import Data.Maybe

funcMap f g llist = f (\list -> map g list) llist
mapMap = funcMap map
concatMapMap = funcMap concatMap

between n lo hi = n >= lo && n < hi
inRect x y left right top bot = between x left right && between y top bot

almostZero x = abs x < 0.5
absMax x y = if abs x > abs y then x else y
absMin x y = if abs x < abs y then x else y

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
    | Circle
    { pos :: Point
    , radius :: Int
    }
    deriving (Show)

rect x y w h = Rect (Point x y) (Point w h)
circle x y r = Circle (Point x y) r

data Grid = Grid
    { width :: Int
    , height :: Int
    , shapes :: [Shape]
    }
    deriving (Show)

emptyGrid w h = Grid w h []

data Tile = Floor | Wall
    deriving (Show, Eq)

data Dungeon = Dungeon
    { tiles :: [[Tile]]
    }
    deriving (Show)

emptyMap w h = Dungeon . replicate h . replicate w $ Wall

showTile Floor = '.'
showTile Wall = '#'

dimensions llist =
    let h = length llist
    in if h == 0
       then (0, 0)
       else let w = length $ head llist
            in (w, h)

dungeonDim (Dungeon tiles) = dimensions tiles

tileAt (Dungeon tiles) (Point x y)
    | not $ inRect x y 0 w 0 h = Nothing
    | otherwise = Just ((tiles !! y) !! x)
    where (w, h) = dimensions tiles

isWall d p = (== Wall) . fromMaybe Wall $ tileAt d p

isLit d p = any (not . isWall d) neighbors
    where neighbors = concatMapMap (/+/ p) pointDiffs
          pointDiffs = pointRange (-1) 1 (-1) 1

showDungeon (Dungeon tiles) = concatMap showLine tiles
    where showLine line = map showTile line ++ "\n"

showLitDungeon dungeon@(Dungeon tiles) = concatMap showLine points
    where showLine line = map showPoint line ++ "\n"
          points = pointGrid w h
          (w, h) = dimensions tiles
          showPoint point
            | not $ isLit dungeon point = ' '
            | otherwise = fromMaybe ' ' $ tileAt dungeon point >>= (\t -> Just . showTile $ t)

pointRange xlo xhi ylo yhi = [[Point x y | x <- [xlo..xhi]] | y <- [ylo..yhi]]
pointGrid w h = pointRange 0 (w-1) 0 (h-1)

gridToDungeon grid = Dungeon tiles
    where tiles = mapMap pointFilled points
          pointFilled p = if isFilled grid p then Floor else Wall
          points = pointGrid w h
          w = width grid
          h = height grid

contains :: Shape -> Point -> Bool
contains (Rect pos size) (Point x y) = inRect x y left right top bot
    where Point left top = pos
          Point right bot = pos /+/ size
contains (Circle center r) point = dist <= r' - 0.5
    where diff = point /-/ center
          dist = magnitude diff
          r' = fromIntegral r

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
    let funcs =
            [ rect 1 1 3 4
            , rect 5 5 5 5
            , rect 2 2 10 1
            , circle 16 8 4
            , circle 32 8 12
            , circle 80 50 50
            ]
        grid = Grid 80 24 funcs
    putStr . showLitDungeon . gridToDungeon $ grid
