module View exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, input, label, table, td, text, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model, Page(..))
import PulsarModel exposing (SubscriptionName, Topic, TopicName, subscriptionNameAsString, topicNameAsString)
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


viewListTopic : Topic -> Html msg
viewListTopic topic =
    tr [] [ td [] [ a [ href <| topicUrl topic ] [ text <| topicNameAsString topic.name ] ] ]


viewTopic :
    { a
        | topicName : TopicName
        , version : Maybe Int
        , creationDate : Maybe String
        , modificationDate : Maybe String
        , subscriptions : Maybe (List SubscriptionName)
    }
    -> Html msg
viewTopic { topicName, version, creationDate, modificationDate, subscriptions } =
    table []
        [ tr [] [ td [] [ text "name" ], td [] [ text "version" ], td [] [ text "created" ], td [] [ text "modified" ] ]
        , tr [] [ td [] [ text <| topicNameAsString topicName ], td [] [ text <| (version |> Maybe.map String.fromInt |> Maybe.withDefault "") ], td [] [ text <| Maybe.withDefault "" creationDate ], td [] [ text <| Maybe.withDefault "" modificationDate ] ]
        , tr [] [ td [] [ viewSubscriptions subscriptions ] ]
        ]


viewSubscriptions : Maybe (List SubscriptionName) -> Html msg
viewSubscriptions subscriptions =
    table [] <|
        [ tr [] [ td [] [ text "subscriptions" ] ] ]
            ++ (subscriptions
                    |> Maybe.withDefault []
                    |> List.map
                        (\subscription ->
                            tr [] [ td [] [ text <| subscriptionNameAsString subscription ] ]
                        )
               )


viewSecret =
    div []
        [ input [ onInput Secret ] []
        , button [ onClick SecretSubmit ] [ text "check" ]
        ]
