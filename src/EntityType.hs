module EntityType where

data EntityType =
      NoType
    | PlayerType
    | EnemyType
    | BulletType
    deriving (Eq, Show)

data EntityMessageSend r =
      Message r EntityMessage
    | Broadcast BroadcastMessage

data EntityMessage =
      NoMessage
    | DamageMessage Int
