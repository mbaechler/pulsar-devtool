module Update exposing (Msg(..), subscriptions, topicUrl, update)

import Browser
import Browser.Navigation as Navigation
import Model exposing (Model, Page(..), clearToken, withCurrentPage, withStatus, withToken, withTopics)
import Pulsar exposing (makeToken)
import PulsarCommands exposing (InternalInfo, loadTopicInternalInfo, loadTopics)
import PulsarModel exposing (Topic)
import RouteBuilder exposing (Route, dynamic, root, s, static, string)
import Secret exposing (Msg(..), savePulsarToken)
import Url
import Url.Parser exposing (Parser, oneOf, parse)


type alias FetchListResult =
    List Topic


type alias FetchTopicInternalInfoResult =
    InternalInfo


type Msg
    = FetchListPulsar
    | FetchListDone FetchListResult
    | FetchListFailed
    | FetchTopicInternalInfoDone FetchTopicInternalInfoResult
    | FetchTopicInternalInfoFailed
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

        FetchTopicInternalInfoDone info ->
            case model.currentPage of
                Loading ->
                    model |> withNoCommand

                ListPage ->
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
                        |> withNoCommand

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


type alias TopicPageModel =
    { topicName : String }


topicRoute : Route TopicPageModel Page
topicRoute =
    root |> s "topic" |> string .topicName |> dynamic TopicPageModel


topicUrl topic =
    topicRoute.toString { topicName = topic.name }


topicParser =
    topicRoute.toParser (\topicPageModel -> TopicPage { topicName = topicPageModel.topicName, creationDate = Nothing, modificationDate = Nothing, version = Nothing })


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
        [ topicParser, listParser, secretParser ]


subscriptions _ =
    Secret.subscriptions |> Sub.map LocalStorage
