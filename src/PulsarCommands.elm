module PulsarCommands exposing (PulsarConfig, internalInfoDecoder, loadTopics, topicsDecoder, loadTopicInternalInfo, InternalInfo)

import Http exposing (Body, Expect, Header)
import Json.Decode as Decode
import Parser exposing ((|.), (|=))
import PulsarModel exposing (Mode(..), Topic)


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


type alias Request msg =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }


loadTopics f pulsar =
    getRequest
        { url = String.join "/" [ pulsar.httpUrl, "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace ]
        , expect = Http.expectJson f topicsDecoder
        }
        |> withBearerToken pulsar.token
        |> Http.request


loadTopicInternalInfo f pulsar topic =
    -- FIXME: urlencode topic
    getRequest
        { url = String.join "/" [ pulsar.httpUrl, "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace, topic, "internal-info" ]
        , expect = Http.expectJson f internalInfoDecoder
        }
        |> withBearerToken pulsar.token
        |> Http.request


getRequest : { url : String, expect : Expect msg } -> Request msg
getRequest { url, expect } =
    { method = "GET"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = expect
    , timeout = Nothing
    , tracker = Nothing
    }


withBearerToken token =
    String.join " " [ "Bearer", token ]
        |> Http.header "Authorization"
        |> prependHeader


prependHeader header request =
    { request | headers = header :: request.headers }


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
        |> Decode.map (List.filterMap toTopic)


type alias InternalInfo =
    { version : Int
    , creationDate : String
    , modificationDate : String
    }


internalInfoDecoder : Decode.Decoder InternalInfo
internalInfoDecoder =
    Decode.map3 InternalInfo
        (Decode.field "version" Decode.int)
        (Decode.field "creationDate" Decode.string)
        (Decode.field "modificationDate" Decode.string)



--FIXME we'd rather keep failing parsings for warning/logs
