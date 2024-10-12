module Model exposing (Model, Page(..), clearStatus, init, withCurrentPage, withSecret, withStatus, withTopics)

import Browser.Navigation as Navigation
import PulsarCommands exposing (PulsarConfig, PulsarToken, makeToken)
import PulsarModel exposing (Topic)


type Page
    = Loading
    | SecretPage
    | ListPage
    | TopicPage
        { topicName : String
        , version : Maybe Int
        , creationDate : Maybe String
        , modificationDate : Maybe String
        }


type alias Model =
    { pulsarConfig : PulsarConfig
    , key : Navigation.Key
    , status : String
    , topics : List Topic
    , currentPage : Page
    , secret : PulsarToken
    }


init key pulsar =
    { key = key
    , pulsarConfig = pulsar
    , status = "nothing"
    , topics = []
    , currentPage = Loading
    , secret = makeToken ""
    }


withStatus : String -> Model -> Model
withStatus status model =
    { model | status = status }


clearStatus : Model -> Model
clearStatus =
    withStatus ""


withTopics : List Topic -> Model -> Model
withTopics topics model =
    { model | topics = topics }


withCurrentPage : Page -> Model -> Model
withCurrentPage page model =
    { model | currentPage = page }


withSecret : String -> Model -> Model
withSecret secret model =
    { model | secret = makeToken secret }
