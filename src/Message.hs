module Message where

import Health

data EntityMessageSend r =
      Message r EntityMessage
    | Broadcast BroadcastMessage

data EntityMessage =
      NoMessage
    | DamageMessage Int

data BroadcastMessage =
      NoBroadcast
    | PlayerHealthUpdated Health
