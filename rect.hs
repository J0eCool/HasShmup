module Rect where

import Vec

data Rect = Rect Vec2f Vec2f
    deriving (Eq, Show)

rect x y w h = Rect (Vec2 x y) (Vec2 w h)

rectsOverlap r1@(Rect (Vec2 x1 y1) (Vec2 w1 h1))
             r2@(Rect (Vec2 x2 y2) (Vec2 w2 h2)) = overlap
    where l1 = x1 - hw1
          r1 = x1 + hw1
          u1 = y1 + hh1
          d1 = y1 - hh1
          l2 = x2 - hw2
          r2 = x2 + hw2
          u2 = y2 + hh2
          d2 = y2 - hh2
          hw1 = w1 / 2
          hh1 = h1 / 2
          hw2 = w2 / 2
          hh2 = h2 / 2
          overlap = l1 < r2 &&
                    r1 > l2 &&
                    u1 > d2 &&
                    d1 < u2
