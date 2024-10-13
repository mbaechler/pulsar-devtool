module PulsarCommands exposing (InternalInfo, PulsarConfig, internalInfoDecoder, loadTopicInternalInfo, loadTopics, topicsDecoder)

import Http exposing (Body, Expect, Header)
import Json.Decode as Decode exposing (Error)
import Json.Decode.Pipeline as Decode
import Parser exposing ((|.), (|=))
import Pulsar
import PulsarModel exposing (Mode(..), Topic)
import Time exposing (Posix, millisToPosix)
import Url.Builder


type alias PulsarConfig =
    { binaryPort : String
    , binaryUrl : String
    , hostname : String
    , httpPort : String
    , httpUrl : String
    , namespace : String
    , tenant : String
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


loadTopics f pulsar token =
    getRequest
        { url = Url.Builder.crossOrigin pulsar.httpUrl [ "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace ] []
        , expect = Http.expectJson f topicsDecoder
        }
        |> withBearerToken token
        |> Http.request


loadTopicInternalInfo f pulsar topic token =
    getRequest
        { url = Url.Builder.crossOrigin pulsar.httpUrl [ "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace, topic, "internal-info" ] []
        , expect = Http.expectJson f internalInfoDecoder
        }
        |> withBearerToken token
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
    String.join " " [ "Bearer", Pulsar.tokenAsString token ]
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
    , ledgers : List Ledger
    }


type alias Ledger =
    { ledgerId : Int
    , entries : Maybe Int
    , size : Maybe Int
    , timestamp : Posix
    , isOffloaded : Bool
    }


internalInfoDecoder : Decode.Decoder InternalInfo
internalInfoDecoder =
    Decode.succeed InternalInfo
        |> Decode.required "version" Decode.int
        |> Decode.required "creationDate" Decode.string
        |> Decode.required "modificationDate" Decode.string
        |> Decode.required "ledgers" (Decode.list ledgerDecoder)


ledgerDecoder : Decode.Decoder Ledger
ledgerDecoder =
    Decode.succeed Ledger
        |> Decode.required "ledgerId" Decode.int
        |> Decode.optional "entries" (Decode.nullable Decode.int) Nothing
        |> Decode.optional "size" (Decode.nullable Decode.int) Nothing
        |> Decode.required "timestamp" (Decode.int |> Decode.map millisToPosix)
        |> Decode.required "isOffloaded" Decode.bool



--FIXME we'd rather keep failing parsings for warning/logs
