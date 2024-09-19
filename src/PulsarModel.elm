module PulsarModel exposing (Mode(..), Topic)

type Mode
    = NonPersistent
    | Persistent

type alias Topic =
    { mode : Mode
    , orga : String
    , namespace : String
    , name : String
    }
