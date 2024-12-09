module View exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, input, label, pre, table, td, text, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model, Page(..))
import Pulsar.Protocol.Messages exposing (Message)
import PulsarModel exposing (Subscription, SubscriptionName, Topic, TopicName, subscriptionNameAsString, topicNameAsString)
import Update exposing (Msg(..), messagesUrl, topicUrl)


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

        MessagesPage topic ->
            viewMessage topic

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
    tr []
        [ td []
            [ a [ href <| topicUrl topic ] [ text <| topicNameAsString topic.name ]
            , a [ href <| messagesUrl topic ] [ text "view messages" ]
            ]
        ]


viewTopic :
    { a
        | topicName : TopicName
        , version : Maybe Int
        , creationDate : Maybe String
        , modificationDate : Maybe String
        , subscriptions : Maybe (List Subscription)
    }
    -> Html msg
viewTopic { topicName, version, creationDate, modificationDate, subscriptions } =
    table []
        [ tr [] [ td [] [ text "name" ], td [] [ text "version" ], td [] [ text "created" ], td [] [ text "modified" ] ]
        , tr [] [ td [] [ text <| topicNameAsString topicName ], td [] [ text <| (version |> Maybe.map String.fromInt |> Maybe.withDefault "") ], td [] [ text <| Maybe.withDefault "" creationDate ], td [] [ text <| Maybe.withDefault "" modificationDate ] ]
        , tr [] [ td [] [ viewSubscriptions subscriptions ] ]
        ]


viewMessage : { topicName : TopicName, message : Maybe Message } -> Html msg
viewMessage { topicName, message } =
    div
        []
        [ text <| "messages for topic " ++ topicNameAsString topicName
        , pre [] [ text (message |> Maybe.map .content |> Maybe.withDefault "") ]
        , text <| "size " ++ (message |> Maybe.map .size |> Maybe.map String.fromInt |> Maybe.withDefault "")
        ]


viewSubscriptions : Maybe (List Subscription) -> Html msg
viewSubscriptions subscriptions =
    table [] <|
        [ tr [] [ td [] [ text "subscriptions" ] ]
        , tr [] [ td [] [ text "name" ], td [] [ text "durable" ], td [] [ text "status" ] ]
        ]
            ++ (subscriptions
                    |> Maybe.withDefault []
                    |> List.map
                        (\subscription ->
                            tr []
                                [ td []
                                    [ text <| subscriptionNameAsString subscription ]
                                , td []
                                    [ text <|
                                        if subscription.durable then
                                            "Durable"

                                        else
                                            "Non durable"
                                    ]
                                , td []
                                    [ text <|
                                        if subscription.hasConsumers then
                                            "Consuming"

                                        else
                                            "Stalled"
                                    ]
                                ]
                        )
               )


viewSecret =
    div []
        [ input [ onInput Secret ] []
        , button [ onClick SecretSubmit ] [ text "check" ]
        ]
