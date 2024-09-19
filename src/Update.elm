module Update exposing (Msg(..), update)

import Model exposing (Model, withStatus, withTopics)
import PulsarCommands exposing (loadTopics)
import PulsarModel exposing (Topic)


type alias FetchResult =
    List Topic


type Msg
    = FetchPulsar
    | Done FetchResult
    | FetchFailed


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
