module Update exposing (Msg(..), messagesUrl, subscriptions, topicUrl, update)

import Browser
import Browser.Navigation as Navigation
import Model exposing (Model, Page(..), clearToken, withCurrentPage, withStatus, withSubscriptions, withToken, withTopics)
import Pulsar exposing (makeToken)
import Pulsar.Protocol.TopicInternalInfo exposing (InternalInfo)
import Pulsar.Protocol.TopicStats exposing (TopicStats)
import PulsarCommands exposing (loadLatestMessage, loadLatestMessages, loadTopicInternalInfo, loadTopics, topicStats)
import PulsarModel exposing (SubscriptionName, Topic, makeTopicName, topicNameAsString)
import RouteBuilder exposing (Route, dynamic, root, s, static, string)
import Secret exposing (Msg(..), savePulsarToken)
import Url
import Url.Parser exposing (Parser, oneOf, parse)


type alias FetchListResult =
    List Topic


type alias FetchTopicInternalInfoResult =
    InternalInfo


type alias FetchMessagesResult =
    List
        { content : String
        , size : Int
        , sequenceId : Int
        }


type Msg
    = FetchListPulsar
    | FetchListDone FetchListResult
    | FetchListFailed
    | FetchTopicInternalInfoDone FetchTopicInternalInfoResult
    | FetchTopicInternalInfoFailed
    | FetchMessagesDone FetchMessagesResult
    | FetchMessagesFailed
    | FetchTopicStatsDone TopicStats
    | FetchTopicStatsFailed
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | LocalStorage Secret.Msg
    | Secret String
    | SecretSubmit
    | AuthSucceed
    | AuthFailure


withCommand : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCommand cmd model =
    ( model, cmd )


withNoCommand : Model -> ( Model, Cmd Msg )
withNoCommand =
    withCommand Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchListDone result ->
            model
                |> withStatus (String.join " / " [ "fetching done" ])
                |> withTopics result
                |> withNoCommand

        FetchListPulsar ->
            model
                |> withStatus "fetching in progress"
                |> withTopics []
                |> withCommand
                    (loadTopics
                        (\result ->
                            case result of
                                Err _ ->
                                    FetchListFailed

                                Ok value ->
                                    FetchListDone value
                        )
                        model.pulsarConfig
                        model.token
                    )

        FetchListFailed ->
            model
                |> withStatus (String.join " / " [ "fetch list failed" ])
                |> withNoCommand

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    model
                        |> withStatus ("want to go to " ++ Url.toString url)
                        |> withCommand (Navigation.pushUrl model.key <| Url.toString url)

                Browser.External url ->
                    model
                        |> withCommand (Navigation.load url)

        UrlChanged url ->
            case parse routes url of
                -- there is no url for "loading", parsing can't return this page
                Just Loading ->
                    model |> withNoCommand

                Just SecretPage ->
                    model
                        |> withCurrentPage SecretPage
                        |> withNoCommand

                Just ListPage ->
                    model
                        |> withCurrentPage ListPage
                        |> withNoCommand

                Nothing ->
                    model |> withNoCommand

                Just (MessagesPage topic) ->
                    model
                        |> withCurrentPage (MessagesPage topic)
                        |> withCommand
                            (loadLatestMessages
                                (\result ->
                                    case result of
                                        Err _ ->
                                            FetchMessagesFailed

                                        Ok value ->
                                            FetchMessagesDone value
                                )
                                (List.range 1 10)
                                model.pulsarConfig
                                topic.topicName
                                model.token
                            )

                Just (TopicPage topic) ->
                    model
                        |> withCurrentPage (TopicPage topic)
                        |> withCommand
                            (loadTopicInternalInfo
                                (\result ->
                                    case result of
                                        Err _ ->
                                            FetchTopicInternalInfoFailed

                                        Ok value ->
                                            FetchTopicInternalInfoDone value
                                )
                                model.pulsarConfig
                                topic.topicName
                                model.token
                            )

        -- FIXME
        FetchMessagesDone messages ->
            case model.currentPage of
                Loading ->
                    model |> withNoCommand

                ListPage ->
                    model |> withNoCommand

                MessagesPage page ->
                    { model
                        | currentPage = MessagesPage { page | messages = messages }
                    }
                        |> withNoCommand

                TopicPage topicPageModel ->
                    model |> withNoCommand

                SecretPage ->
                    model |> withNoCommand

        FetchMessagesFailed ->
            model
                |> withStatus (String.join " / " [ "fetch messages failed" ])
                |> withNoCommand

        FetchTopicInternalInfoDone info ->
            case model.currentPage of
                Loading ->
                    model |> withNoCommand

                ListPage ->
                    model |> withNoCommand

                MessagesPage _ ->
                    model |> withNoCommand

                TopicPage topicPageModel ->
                    model
                        |> withCurrentPage
                            (TopicPage
                                { topicPageModel
                                    | version = Just info.version
                                    , creationDate = Just info.creationDate
                                    , modificationDate = Just info.modificationDate
                                }
                            )
                        |> withCommand
                            (topicStats
                                (\result ->
                                    case result of
                                        Err _ ->
                                            FetchTopicStatsFailed

                                        Ok subscriptionNames ->
                                            FetchTopicStatsDone subscriptionNames
                                )
                                model.pulsarConfig
                                topicPageModel.topicName
                                model.token
                            )

                SecretPage ->
                    model |> withNoCommand

        FetchTopicInternalInfoFailed ->
            model
                |> withStatus (String.join " / " [ "fetch internalinfo failed" ])
                |> withNoCommand

        LocalStorage (TokenLoadSuccess (Just token)) ->
            model
                |> withToken token
                |> update SecretSubmit

        LocalStorage (TokenLoadSuccess Nothing) ->
            model
                |> clearToken
                |> update SecretSubmit

        LocalStorage (TokenLoadFailure message) ->
            model
                |> withStatus ("unable to read secret from secret management / " ++ message)
                |> withNoCommand

        LocalStorage (UnexpectedEvent message) ->
            model
                |> withStatus ("unexpected error from secret management / " ++ message)
                |> withNoCommand

        Secret string ->
            model |> withToken (makeToken string) |> withNoCommand

        SecretSubmit ->
            model
                |> withCommand
                    (loadTopics
                        (\result ->
                            case result of
                                Err _ ->
                                    AuthFailure

                                Ok value ->
                                    AuthSucceed
                        )
                        model.pulsarConfig
                        model.token
                    )

        AuthSucceed ->
            model
                |> withCurrentPage ListPage
                |> withCommand (savePulsarToken model.token)

        AuthFailure ->
            model
                |> withCurrentPage SecretPage
                |> withStatus "invalid credentials"
                |> withNoCommand

        FetchTopicStatsDone topicStats ->
            let
                buildSubscription fromApi =
                    { name = fromApi.name
                    , durable = fromApi.durable
                    , hasConsumers = not <| List.isEmpty fromApi.consumers
                    }

                page =
                    model.currentPage
                        |> withSubscriptions (topicStats |> .subscriptions |> List.map buildSubscription)
            in
            model
                |> withCurrentPage page
                |> withNoCommand

        FetchTopicStatsFailed ->
            model
                |> withStatus "get topic stats failed"
                |> withNoCommand


type alias TopicPageModel =
    { topicName : String }


topicRoute : Route TopicPageModel Page
topicRoute =
    root |> s "topic" |> string .topicName |> dynamic TopicPageModel


topicUrl : Topic -> String
topicUrl topic =
    topicRoute.toString { topicName = topicNameAsString topic.name }


topicParser =
    topicRoute.toParser (\topicPageModel -> TopicPage { topicName = makeTopicName topicPageModel.topicName, creationDate = Nothing, modificationDate = Nothing, version = Nothing, subscriptions = Nothing })


type alias MessagesPageModel =
    { topicName : String }


messagesRoute : Route TopicPageModel Page
messagesRoute =
    root |> s "topic" |> string .topicName |> s "messages" |> dynamic MessagesPageModel


messagesUrl : Topic -> String
messagesUrl topic =
    messagesRoute.toString { topicName = topicNameAsString topic.name }


messagesParser =
    messagesRoute.toParser (\topicPageModel -> MessagesPage { topicName = makeTopicName topicPageModel.topicName, messages = [] })


listRoute : Route () Page
listRoute =
    root |> static


listUrl =
    listRoute.toString ()


listParser =
    listRoute.toParser (\_ -> ListPage)


secretRoute : Route () Page
secretRoute =
    root |> s "secret" |> static


secretUrl =
    secretRoute.toString ()


secretParser =
    secretRoute.toParser (\_ -> SecretPage)


routes =
    oneOf
        [ topicParser
        , messagesParser
        , listParser
        , secretParser
        ]


subscriptions _ =
    Secret.subscriptions |> Sub.map LocalStorage
