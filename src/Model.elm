module Model exposing (Model, Page(..), clearStatus, clearToken, init, withCurrentPage, withStatus, withToken, withTopics)

import Browser.Navigation as Navigation
import Pulsar exposing (PulsarToken, makeToken)
import PulsarCommands exposing (PulsarConfig)
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
    , token : PulsarToken
    }


init key pulsar =
    { key = key
    , pulsarConfig = pulsar
    , status = "nothing"
    , topics = []
    , currentPage = Loading
    , token = makeToken ""
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


withToken : PulsarToken -> Model -> Model
withToken token model =
    { model | token = token }


clearToken : Model -> Model
clearToken model =
    { model | token = makeToken "" }
