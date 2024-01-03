module View exposing (view)

import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        topicList =
            table [] (List.map (\topic -> tr [] [ td [] [ text topic.name ] ]) model.topics)
    in
    div []
        [ button [ onClick FetchPulsar ] [ text "send request" ]
        , div [] [ text model.status ]
        , div [] [ topicList ]
        ]
