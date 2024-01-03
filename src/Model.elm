module Model exposing (Model, Topic, init, withStatus, withTopics)

import PulsarCommands exposing (PulsarConfig)


type alias Topic =
    { name : String }


type alias Model =
    { pulsarConfig : PulsarConfig
    , status : String
    , topics : List Topic
    }


init pulsar =
    { pulsarConfig = pulsar, status = "nothing", topics = [] }


withStatus : String -> Model -> Model
withStatus status model =
    { model | status = status }


withTopics : List Topic -> Model -> Model
withTopics topics model =
    { model | topics = topics }
