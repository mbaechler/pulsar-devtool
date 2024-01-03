module PulsarCommands exposing (PulsarConfig, loadTopics)

import Http
import Json.Decode as Decode


type alias PulsarConfig =
    { binaryPort : String
    , binaryUrl : String
    , hostname : String
    , httpPort : String
    , httpUrl : String
    , namespace : String
    , tenant : String
    , token : String
    }


loadTopics f pulsar =
    Http.request
        { method = "GET"
        , headers = [ token pulsar.token ]
        , url = String.join "/" [ pulsar.httpUrl, "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                f
                topicsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


token t =
    Http.header "Authorization" <| String.join " " [ "Bearer", t ]


topicsDecoder : Decode.Decoder (List String)
topicsDecoder =
    Decode.list Decode.string
