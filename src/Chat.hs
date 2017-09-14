{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Chat where

import           Control.Auto

import           Types


data a :<|>: b
data a :> b

data Optional a
data Parse a

type AppIdeal o = ("play" :<|>: "add") :> Parse Track :> o
            :<|>: "queue" :> o
            :<|>: ("np" :<|>: "nowplaying") :> o
            :<|>: "skip" :> o
            :<|>: "summon" :> o
            :<|>: "volume" :> (Optional (Parse Int)) :> o
