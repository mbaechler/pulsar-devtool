module Update exposing (Msg(..), topicUrl, update)

import Browser
import Browser.Navigation as Navigation
import Model exposing (Model, Page(..), clearStatus, withCurrentPage, withStatus, withTopics)
import PulsarCommands exposing (loadTopics)
import PulsarModel exposing (Topic)
import RouteBuilder exposing (Route, dynamic, root, s, string)
import Url
import Url.Parser exposing (Parser, oneOf, parse)


type alias FetchResult =
    List Topic


type Msg
    = FetchPulsar
    | Done FetchResult
    | FetchFailed
    | Details Topic
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
        Done result ->
            model
                |> withStatus (String.join " / " [ "fetching done" ])
                |> withTopics result
                |> withNoCommand

        FetchPulsar ->
            model
                |> withStatus "fetching in progress"
                |> withTopics []
                |> withCommand
                    (loadTopics
                        (\result ->
                            case result of
                                Err _ ->
                                    FetchFailed

                                Ok value ->
                                    Done value
                        )
                        model.pulsarConfig
                    )

        FetchFailed ->
            model
                |> withStatus (String.join " / " [ "fetch failed" ])
                |> withNoCommand

        Details topic ->
            model
                |> clearStatus
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
                    model |> withNoCommand

                Nothing ->
                    model |> withNoCommand

                Just (TopicPage topic) ->
                    model
                        |> withCurrentPage (TopicPage topic)
                        |> withNoCommand


type alias TopicPageModel =
    { topicName : String }


topicRoute : Route TopicPageModel Page
topicRoute =
    root |> s "topic" |> string .topicName |> dynamic TopicPageModel


topicUrl topic =
    topicRoute.toString { topicName = topic.name }


topicParser =
    topicRoute.toParser TopicPage


routes =
    oneOf
        [ topicParser ]
