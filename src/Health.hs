{-# LANGUAGE TemplateHaskell #-}

module Health where

import Control.Lens

import Draw
import Math.Math
import Math.Rect
import Time

data Health = Health
    { _currentHealth :: Int
    , _maxHealth :: Int
    , _invincibleTimer :: Float
    , _maxInvincibleTime :: Float
    }
    deriving (Eq)
makeLenses ''Health

modifyIf cond f x = if cond then f x else x

newHealth health invinTime = Health health health 0 invinTime
updateHealth dT damageTaken health = health
    & currentHealth %~ modifyIf tookDamage (subtract damageTaken)
    & invincibleTimer %~ updateTimer tookDamage dT (health ^. maxInvincibleTime)
    where tookDamage = damageTaken > 0 && not (isInvincible health)

isInvincible health = health ^. invincibleTimer > 0
isDead health = health ^. currentHealth <= 0
isNotFlashing health = not invin || isEvenFrame
    where invin = isInvincible health
          isEvenFrame = even $ floor (health ^. invincibleTimer * 20) `mod` 2

drawHealthBar maxWidth health = do
    colorRGB $ RGB 0.5 0 0
    drawTopLeftRect $ rect x y maxWidth height
    colorRGB $ RGB 1 0 0
    drawTopLeftRect $ rect x y width height
    where (x, y) = (-0.9, -0.9)
          height = 0.1
          width = pct * maxWidth
          pct = clamp 0 1 $ f currentHealth / f maxHealth
          f = fromIntegral . (health ^.)