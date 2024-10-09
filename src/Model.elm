module Model exposing (Model, Page(..), clearStatus, init, withCurrentPage, withStatus, withTopics)

import Browser.Navigation as Navigation
import PulsarCommands exposing (PulsarConfig)
import PulsarModel exposing (Topic)


type Page
    = ListPage
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
    }


init key pulsar =
    { key = key
    , pulsarConfig = pulsar
    , status = "nothing"
    , topics = []
    , currentPage = ListPage
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
