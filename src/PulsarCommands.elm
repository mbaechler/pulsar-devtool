module PulsarCommands exposing (PulsarConfig, listSubscriptions, loadTopicInternalInfo, loadTopics)

import Http exposing (Body, Expect, Header)
import Json.Decode as Decode exposing (Error)
import Pulsar
import Pulsar.Protocol.TopicInternalInfo exposing (InternalInfo, internalInfoDecoder)
import Pulsar.Protocol.Topics exposing (topicsDecoder)
import PulsarModel exposing (Mode(..), SubscriptionName, Topic, TopicName, makeSubscriptionName, topicNameAsString)
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


loadTopics : (Result Http.Error (List Topic) -> msg) -> PulsarConfig -> Pulsar.PulsarToken -> Cmd msg
loadTopics f pulsar token =
    getRequest
        { url = Url.Builder.crossOrigin pulsar.httpUrl [ "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace ] []
        , expect = Http.expectJson f topicsDecoder
        }
        |> withBearerToken token
        |> Http.request


loadTopicInternalInfo : (Result Http.Error InternalInfo -> msg) -> PulsarConfig -> TopicName -> Pulsar.PulsarToken -> Cmd msg
loadTopicInternalInfo f pulsar topic token =
    getRequest
        { url = Url.Builder.crossOrigin pulsar.httpUrl [ "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace, topicNameAsString topic, "internal-info" ] []
        , expect = Http.expectJson f internalInfoDecoder
        }
        |> withBearerToken token
        |> Http.request


listSubscriptions : (Result Http.Error (List SubscriptionName) -> msg) -> PulsarConfig -> TopicName -> Pulsar.PulsarToken -> Cmd msg
listSubscriptions f pulsar topic token =
    getRequest
        { url = Url.Builder.crossOrigin pulsar.httpUrl [ "admin", "v2", "persistent", pulsar.tenant, pulsar.namespace, topicNameAsString topic, "subscriptions" ] []
        , expect = Http.expectJson f (Decode.list (Decode.string |> Decode.map makeSubscriptionName))
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



--FIXME we'd rather keep failing parsings for warning/logs
