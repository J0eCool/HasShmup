{-# LANGUAGE ExistentialQuantification #-}

class Updateable a where
    update :: a -> a
    update = id

data Box = forall a . (Show a, Updateable a) => B a

instance Show Box where
    show (B x) = show x
instance Updateable Box where
    update (B x) = B (update x)

instance Updateable Int where
    update = (+ 1)
instance Updateable Char where
    update = succ
instance (Updateable a) => Updateable [a] where
    update = map update

main = mapM_ print boxed
    where boxed = map update
            [ B (3 :: Int)
            , B "guts"
            , B ([1, 2] :: [Int])
            ]
