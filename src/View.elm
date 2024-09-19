module View exposing (view)

import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchPulsar ] [ text "send request" ]
        , div [] [ text model.status ]
        , div [] [ showTopicList model.topics ]
        ]

showTopicList topics =
    topics
    |> List.map showTopic
    |> table []

showTopic topic = tr [] [ td [] [ text topic.name ] ]