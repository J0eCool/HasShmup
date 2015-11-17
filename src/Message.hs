module Message where

data EntityMessageSend r =
      Message r EntityMessage
    | Broadcast BroadcastMessage

data EntityMessage =
      NoMessage
    | DamageMessage Int
