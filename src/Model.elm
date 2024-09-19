module Model exposing (Model, init, withStatus, withTopics)

import PulsarCommands exposing (PulsarConfig)
import PulsarModel exposing (Topic)

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
