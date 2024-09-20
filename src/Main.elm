module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Model exposing (Model)
import PulsarCommands exposing (PulsarConfig)
import Update exposing (Msg(..), update)
import Url
import View exposing (view)


pulsar : PulsarConfig
pulsar =
    { binaryPort = "2002"
    , binaryUrl = "pulsar+ssl://c2-pulsar-clevercloud-customers.services.clever-cloud.com:2002"
    , hostname = "c2-pulsar-clevercloud-customers.services.clever-cloud.com"
    , httpPort = "2000"
    , httpUrl = "http://localhost:8000/api"
    , namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3"
    , tenant = "orga_e8400f27-5fb2-4a31-933e-539e63507291"
    , token = ""
    }


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( Model.init key pulsar, Cmd.none )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    UrlRequested


onUrlChange : Url.Url -> Msg
onUrlChange =
    UrlChanged
