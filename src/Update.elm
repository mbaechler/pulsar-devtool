module Update exposing (Msg(..), localstorage, subscriptions, topicUrl, update)

import Browser
import Browser.Navigation as Navigation
import Json.Decode
import LocalStorage exposing (Response(..))
import Model exposing (Model, Page(..), withCurrentPage, withSecret, withStatus, withTopics)
import Ports.LocalStorage as LocalStoragePort exposing (clear, getItem, listKeys, setItem)
import PulsarCommands exposing (InternalInfo, PulsarToken, decodeToken, loadTopicInternalInfo, loadTopics)
import PulsarModel exposing (Topic)
import RouteBuilder exposing (Route, dynamic, root, s, static, string)
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
    | LocalStorage Response
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
                        model.secret
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
                                model.secret
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

        LocalStorage (Item "secret" jsonSecret) ->
            case decodeToken jsonSecret of
                Err message ->
                    model
                        |> withStatus ("unable to decode secret from localstorage / " ++ Json.Decode.errorToString message)
                        |> withNoCommand

                Ok secret ->
                    model
                        |> withSecret secret
                        |> withCurrentPage ListPage
                        |> withNoCommand

        LocalStorage (Item key value) ->
            model
                |> withStatus ("unknown key retrieve from localstorage" ++ key)
                |> withNoCommand

        LocalStorage (ItemNotFound "secret") ->
            model
                |> withStatus "no secret found"
                |> withCommand (Navigation.pushUrl model.key secretUrl)

        LocalStorage (ItemNotFound item) ->
            model |> withNoCommand

        LocalStorage (KeyList list) ->
            model |> withNoCommand

        LocalStorage (Error error) ->
            model |> withNoCommand

        Secret string ->
            model |> withSecret string |> withNoCommand

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
                        model.secret
                    )

        AuthSucceed ->
            model
                |> withCurrentPage ListPage
                |> withCommand (savePulsarSecret model.secret)

        AuthFailure ->
            model
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


localstorage =
    LocalStorage.make getItem setItem clear listKeys "pulsar-devtool"


subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.responseHandler LocalStorage localstorage
        |> LocalStoragePort.response


savePulsarSecret : PulsarToken -> Cmd Msg
savePulsarSecret token =
    LocalStorage.setItem localstorage "secret" (PulsarCommands.encodeToken token)
