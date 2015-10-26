{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity where

class Entity i e | e -> i where
  update :: i -> e -> e
  draw :: e -> IO ()
  entitiesToSpawn ::  i -> e -> [e]
  shouldRemove :: i -> e -> Bool

  update _ = id
  draw _ = return ()
  entitiesToSpawn _ _ = []
  shouldRemove _ _ = False

data EntityBox i = forall e . (Entity i e) => EBox e

--instance Entity Int Char where
--  update 0 c = c
--  update n c = update (n - 1) (succ c)
--instance Show (EntityBox Int) where
--  show (EBox x) = show x
--instance Entity Int (EntityBox Int) where
--  update x (EBox y) = EBox $ update x y
