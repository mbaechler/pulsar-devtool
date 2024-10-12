module View exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, input, label, table, td, text, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model, Page(..))
import PulsarModel exposing (Topic)
import Update exposing (Msg(..), topicUrl)


view : Model -> Document Msg
view model =
    { title = "Pulsar DevTool"
    , body =
        [ div
            []
            [ a [ href "http://perdu.com" ] [ text "let me out" ]
            , div [] [ text <| "current status : " ++ model.status ]
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
            viewTopic topic

        Loading ->
            text "Loading"

        SecretPage ->
            viewSecret


viewList : List Topic -> Html Msg
viewList topics =
    div []
        [ button [ onClick FetchListPulsar ] [ text "send request" ]
        , topics
            |> List.map viewListTopic
            |> table []
        ]


viewListTopic topic =
    tr [] [ td [] [ a [ href <| topicUrl topic ] [ text topic.name ] ] ]


viewTopic :
    { topicName : String
    , version : Maybe Int
    , creationDate : Maybe String
    , modificationDate : Maybe String
    }
    -> Html msg
viewTopic { topicName, version, creationDate, modificationDate } =
    table []
        [ tr [] [ td [] [ text "name" ], td [] [ text "version" ], td [] [ text "created" ], td [] [ text "modified" ] ]
        , tr [] [ td [] [ text topicName ], td [] [ text <| (version |> Maybe.map String.fromInt |> Maybe.withDefault "") ], td [] [ text <| Maybe.withDefault "" creationDate ], td [] [ text <| Maybe.withDefault "" modificationDate ] ]
        ]


viewSecret =
    div []
        [ input [ onInput Secret ] []
        , button [ onClick SecretSubmit ] [ text "check" ]
        ]
