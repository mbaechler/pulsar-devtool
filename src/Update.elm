module Update exposing (Msg(..), topicUrl, update)

import Browser
import Browser.Navigation as Navigation
import Model exposing (Model, Page(..), withCurrentPage, withStatus, withTopics)
import PulsarCommands exposing (InternalInfo, loadTopicInternalInfo, loadTopics)
import PulsarModel exposing (Topic)
import RouteBuilder exposing (Route, dynamic, root, s, string)
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
                Just ListPage ->
                    model
                        |> withCurrentPage ListPage
                        |> withNoCommand

                Nothing ->
                    model |> withNoCommand

                Just (TopicPage topic) ->
                    model
                        |> withCurrentPage (TopicPage topic)
                        |> withCommand (loadTopicInternalInfo
                                                   (\result ->
                                                       case result of
                                                           Err _ ->
                                                               FetchTopicInternalInfoFailed

                                                           Ok value ->
                                                               FetchTopicInternalInfoDone value
                                                   )
                                                   model.pulsarConfig topic.topicName
                                               )

        FetchTopicInternalInfoDone info ->
            case model.currentPage of
                ListPage -> model |> withNoCommand

                TopicPage topicPageModel ->
                    model
                    |> withCurrentPage (TopicPage
                        { topicPageModel
                        | version = Just info.version
                        , creationDate = Just info.creationDate
                        , modificationDate = Just info.modificationDate
                        })
                    |> withNoCommand

        FetchTopicInternalInfoFailed ->
                        model
                            |> withStatus (String.join " / " [ "fetch internalinfo failed" ])
                            |> withNoCommand



type alias TopicPageModel =
    { topicName : String }


topicRoute : Route TopicPageModel Page
topicRoute =
    root |> s "topic" |> string .topicName |> dynamic TopicPageModel


topicUrl topic =
    topicRoute.toString { topicName = topic.name }


topicParser =
    topicRoute.toParser (\topicPageModel -> TopicPage { topicName = topicPageModel.topicName, creationDate = Nothing, modificationDate = Nothing, version = Nothing})


type alias ListPageModel =
    {}


listRoute : Route ListPageModel Page
listRoute =
    root |> dynamic ListPageModel


listParser =
    listRoute.toParser (\_ -> ListPage)


routes =
    oneOf
        [ topicParser, listParser ]
