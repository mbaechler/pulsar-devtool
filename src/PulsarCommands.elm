module PulsarCommands exposing (PulsarConfig, listSubscriptions, loadLatestMessage, loadLatestMessages, loadTopicInternalInfo, loadTopics, topicStats)

import Http exposing (Body, Error(..), Expect, Header, Response(..))
import Json.Decode as Decode exposing (Error)
import Pulsar
import Pulsar.Protocol.Messages exposing (Message, messageDecoder)
import Pulsar.Protocol.TopicInternalInfo exposing (InternalInfo, internalInfoDecoder)
import Pulsar.Protocol.TopicStats exposing (TopicStats, topicStatsDecoder)
import Pulsar.Protocol.Topics exposing (topicsDecoder)
import PulsarModel exposing (Mode(..), SubscriptionName, Topic, TopicName, makeSubscriptionName, topicNameAsString)
import Task exposing (Task)
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


type alias PulsarParams a =
    { a | pulsarConfig : PulsarConfig, token : Pulsar.PulsarToken }


type alias Request msg =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }


type alias RequestTask a =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , resolver : Http.Resolver Http.Error a
    , timeout : Maybe Float
    }


loadTopics : (Result Http.Error (List Topic) -> msg) -> PulsarParams a -> Cmd msg
loadTopics f { pulsarConfig, token } =
    getRequest
        { url = Url.Builder.crossOrigin pulsarConfig.httpUrl [ "admin", "v2", "persistent", pulsarConfig.tenant, pulsarConfig.namespace ] []
        , expect = Http.expectJson f topicsDecoder
        }
        |> withBearerToken token
        |> Http.request


loadTopicInternalInfo : (Result Http.Error InternalInfo -> msg) -> PulsarParams a -> TopicName -> Cmd msg
loadTopicInternalInfo f { pulsarConfig, token } topic =
    getRequest
        { url = Url.Builder.crossOrigin pulsarConfig.httpUrl [ "admin", "v2", "persistent", pulsarConfig.tenant, pulsarConfig.namespace, topicNameAsString topic, "internal-info" ] []
        , expect = Http.expectJson f internalInfoDecoder
        }
        |> withBearerToken token
        |> Http.request


loadLatestMessageTask : PulsarParams a -> TopicName -> Int -> Task Http.Error Message
loadLatestMessageTask { pulsarConfig, token } topic messagePosition =
    let
        request =
            getRequestTask
                { url =
                    Url.Builder.crossOrigin pulsarConfig.httpUrl
                        [ "admin", "v2", "persistent", pulsarConfig.tenant, pulsarConfig.namespace, topicNameAsString topic, "examinemessage" ]
                        [ Url.Builder.string "initialPosition" "latest"
                        , Url.Builder.int "messagePosition" messagePosition
                        ]
                , responseHandler =
                    \response ->
                        case response of
                            BadUrl_ url ->
                                Err (BadUrl url)

                            Timeout_ ->
                                Err Timeout

                            NetworkError_ ->
                                Err NetworkError

                            BadStatus_ metadata _ ->
                                Err (BadStatus metadata.statusCode)

                            GoodStatus_ metadata body ->
                                Result.mapError BadBody (messageDecoder metadata body)
                }
                |> withBearerToken token
    in
    request |> Http.riskyTask


loadLatestMessage : (Result Http.Error Message -> msg) -> PulsarParams a -> TopicName -> Cmd msg
loadLatestMessage f params topic =
    loadLatestMessageTask params topic 1
        |> Task.attempt f


loadLatestMessages : (Result Http.Error (List Message) -> msg) -> PulsarParams a -> TopicName -> List Int -> Cmd msg
loadLatestMessages f params topic range =
    let
        loadMessage =
            loadLatestMessageTask params topic
    in
    range
        |> List.map loadMessage
        |> Task.sequence
        |> Task.attempt f


listSubscriptions : (Result Http.Error (List SubscriptionName) -> msg) -> PulsarParams a -> TopicName -> Cmd msg
listSubscriptions f { pulsarConfig, token } topic =
    getRequest
        { url = Url.Builder.crossOrigin pulsarConfig.httpUrl [ "admin", "v2", "persistent", pulsarConfig.tenant, pulsarConfig.namespace, topicNameAsString topic, "subscriptions" ] []
        , expect = Http.expectJson f (Decode.list (Decode.string |> Decode.map makeSubscriptionName))
        }
        |> withBearerToken token
        |> Http.request


topicStats : (Result Http.Error TopicStats -> msg) -> PulsarParams a -> TopicName -> Cmd msg
topicStats f { pulsarConfig, token } topic =
    getRequest
        { url = Url.Builder.crossOrigin pulsarConfig.httpUrl [ "admin", "v2", "persistent", pulsarConfig.tenant, pulsarConfig.namespace, topicNameAsString topic, "stats" ] []
        , expect = Http.expectJson f topicStatsDecoder
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


getRequestTask : { url : String, responseHandler : Response String -> Result Http.Error a } -> RequestTask a
getRequestTask { url, responseHandler } =
    { method = "GET"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , resolver = Http.stringResolver responseHandler
    , timeout = Nothing
    }


withBearerToken token =
    String.join " " [ "Bearer", Pulsar.tokenAsString token ]
        |> Http.header "Authorization"
        |> prependHeader


prependHeader header request =
    { request | headers = header :: request.headers }



--FIXME we'd rather keep failing parsings for warning/logs
