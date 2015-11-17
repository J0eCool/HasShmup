module Message where

import Health

data EntityMessageSend r =
      Message EntityMessage r
    | Broadcast BroadcastMessage

data EntityMessage =
      NoMessage
    | DamageMessage Int
    | UpgradeMessage

data BroadcastMessage =
      NoBroadcast
    | PlayerHealthUpdated Health

---------------------------------------

messageCount counter = sum . map counter

messageDamageTotal = messageCount damage
    where damage (DamageMessage d) = d
          damage _ = 0

boolToInt True = 1
boolToInt False = 0

isMessageUpgrade UpgradeMessage = True
isMessageUpgrade _ = False
