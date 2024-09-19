module PulsarCommands exposing (Mode(..), PulsarConfig, loadTopics, topicsDecoder, Topic)

import Http
import Json.Decode as Decode
import Parser exposing ((|.), (|=))


type Mode
    = NonPersistent
    | Persistent


type alias Topic =
    { mode : Mode
    , orga : String
    , namespace : String
    , name : String
    }


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


topicParser =
    let
        isNotSlash =
            (/=) '/'

        segment =
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf isNotSlash
                    |. Parser.chompWhile isNotSlash

        orga =
            segment

        namespace =
            segment

        name =
            segment

        mode =
            Parser.oneOf
                [ Parser.map (\_ -> Persistent) <| Parser.keyword "persistent"
                , Parser.map (\_ -> NonPersistent) <| Parser.keyword "non-persistent"
                ]
    in
    Parser.succeed Topic
        |= mode
        |. Parser.symbol "://"
        |= orga
        |. Parser.symbol "/"
        |= namespace
        |. Parser.symbol "/"
        |= name
        |. Parser.end


topicsDecoder : Decode.Decoder (List Topic)
topicsDecoder =
    let
        toTopic : String -> Maybe Topic
        toTopic url =
            url
                |> Parser.run topicParser
                |> Result.toMaybe
    in
    Decode.list Decode.string
        |> Decode.map (List.filterMap toTopic) --FIXME we'd rather keep failing parsings for warning/logs
