module EntityType where

data EntityType =
      NoType
    | PlayerType
    | EnemyType
    | BulletType
    deriving (Eq, Show)
