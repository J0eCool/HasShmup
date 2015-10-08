import Control.Applicative
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Random

funcMap f g llist = f (\list -> map g list) llist

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap = funcMap map

concatMapMap :: (a -> b) -> [[a]] -> [b]
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
    | HorizontalLine
    { pos :: Point
    , lineLength :: Int
    }
    | VerticalLine
    { pos :: Point
    , lineLength :: Int
    }
    deriving (Show)

rect x y w h = Rect (Point x y) (Point w h)
circle x y r = Circle (Point x y) r

data Tile = Floor | Wall
    deriving (Show, Eq)

floorTile True = Floor
floorTile False = Wall

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
    where neighbors = map (/+/ p) pointDiffs
          pointDiffs = concat $ pointRange (-1) 1 (-1) 1

showDungeon (Dungeon tiles) = concatMap showLine tiles
    where showLine line = map showTile line ++ "\n"

showLitDungeon dungeon = concatMap showLine points
    where showLine line = map showPoint line ++ "\n"
          points = pointGrid w h
          (w, h) = dungeonDim dungeon
          showPoint point
            | not $ isLit dungeon point = ' '
            | otherwise = fromMaybe ' ' $ showTile <$> tileAt dungeon point

pointRange xlo xhi ylo yhi = [[Point x y | x <- [xlo..xhi]] | y <- [ylo..yhi]]
pointGrid w h = pointRange 0 (w-1) 0 (h-1)

contains :: Point -> Shape -> Bool
contains (Point x y) (Rect pos size) = inRect x y left right top bot
    where Point left top = pos
          Point right bot = pos /+/ size
contains point (Circle center r) = dist <= r' - 0.5
    where diff = point /-/ center
          dist = magnitude diff
          r' = fromIntegral r
contains (Point x y) (HorizontalLine (Point lx ly) len) = inRect x y lx (lx + len) ly (ly + 1)
contains (Point x y) (VerticalLine (Point lx ly) len) = inRect x y lx (lx + 1) ly (ly + len)

isFilled :: [Shape] -> Point -> Bool
isFilled shapes point = any (contains point) shapes

dungeonWithShapes shapes width height = Dungeon $ mapMap (floorTile . isFilled shapes) points
    where points = pointGrid width height

type RandState a = State StdGen a

randMod :: Int -> RandState Int
randMod modulo = (`mod` modulo) <$> state random

randR :: Int -> Int -> RandState Int
randR lo hi = state $ randomR (lo, hi)

randElem :: [a] -> RandState a
randElem list = do
    let len = length list
    i <- randMod len
    return $ list !! i

randRect :: RandState Shape
randRect = do
    w <- randR 2 12
    h <- randR 2 6
    x <- randR 1 70
    y <- randR 1 40
    return $ rect x y w h


randPoint (Point x y) = Point <$> randMod x <*> randMod y

randPointInside (Rect pos size) = (/+/) <$> randPoint size <*> return pos

intersperseWith _ [] = []
intersperseWith _ [x] = [x]
intersperseWith f (a:b:xs) = a : rs ++ intersperseWith f (b:xs)
    where rs = f a b

connectRoomList rooms = intersperseWith connectRooms rooms
connectRooms a b = [h, v]
    where aPoint@(Point ax ay) = pos a
          bPoint@(Point bx by) = pos b
          hDist = abs (ax - bx)
          vDist = abs (ay - by)
          goRight = ax < bx
          goDown = ay < by
          hPoint@(Point hx hy) = Point (if goRight then ax else ax - hDist) ay
          h = HorizontalLine hPoint (hDist + 1)
          vPoint = Point (if goRight then hx + hDist else hx) (if goDown then ay else ay - vDist)
          v = VerticalLine vPoint (vDist + 1)

randomShapes :: RandState [Shape]
randomShapes = do
    numShapes <- randR 3 5
    rects <- replicateM numShapes randRect
    return $ connectRoomList rects

dungeonFromGrid xRooms yRooms roomWidth roomHeight =
    dungeonWithShapes shapes totalWidth totalHeight
    where totalWidth = xRooms * xDelta + 1
          totalHeight = yRooms * yDelta + 1
          shapes = liftM2 (\x y -> rect x y roomWidth roomHeight) xs ys
          xs = take xRooms [1,(xDelta + 1)..]
          ys = take yRooms [1,(yDelta + 1)..]
          xDelta = roomWidth + 1
          yDelta = roomHeight + 1


dungeonFromSeed seed = dungeonWithShapes shapes 80 48
    where shapes = evalState randomShapes gen
          gen = mkStdGen seed

printDungeonFromSeed = putStr . showLitDungeon . dungeonFromSeed

main :: IO ()
main = do
    curEpoch <- round <$> getPOSIXTime
    printDungeonFromSeed curEpoch
