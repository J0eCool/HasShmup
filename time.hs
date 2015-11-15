module Time where

updateTimer shouldReset dT resetTime timer =
    if shouldReset
    then resetTime
    else timer - dT
