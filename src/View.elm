module View exposing (view)

import Html exposing (Html, a, button, div, table, td, text, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Model exposing (Model, Page(..))
import PulsarModel exposing (Topic)
import Update exposing (Msg(..), topicUrl)


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Pulsar DevTool"
    , body =
        [ div
            []
            [ a [ href "http://perdu.com" ] [ text "let me out" ]
            , button [ onClick FetchPulsar ] [ text "send request" ]
            , div [] [ text model.status ]
            , div [] [ viewPage model ]
            ]
        ]
    }


viewPage : { a | topics : List Topic, currentPage : Page } -> Html Msg
viewPage { topics, currentPage } =
    case currentPage of
        ListPage ->
            viewList topics

        TopicPage topic ->
            viewTopic topic.topicName


viewList : List Topic -> Html Msg
viewList topics =
    topics
        |> List.map viewListTopic
        |> table []


viewListTopic topic =
    tr [] [ td [] [ a [ href <| topicUrl topic ] [ text topic.name ] ] ]


viewTopic : String -> Html msg
viewTopic name =
    tr [] [ td [] [ text name ] ]
