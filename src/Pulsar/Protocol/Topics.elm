module Pulsar.Protocol.Topics exposing (topicsDecoder)

import Json.Decode as Decode exposing (Error)
import Parser exposing ((|.), (|=))
import PulsarModel exposing (Mode(..), SubscriptionName, Topic, TopicName, makeTopicName)


topicParser =
    let
        isNotSlash =
            (/=) '/'

        segment =
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf isNotSlash
                    |. Parser.chompWhile isNotSlash

        orga =
            segment

        namespace =
            segment

        name =
            segment

        mode =
            Parser.oneOf
                [ Parser.map (\_ -> Persistent) <| Parser.keyword "persistent"
                , Parser.map (\_ -> NonPersistent) <| Parser.keyword "non-persistent"
                ]
    in
    Parser.succeed Topic
        |= mode
        |. Parser.symbol "://"
        |= orga
        |. Parser.symbol "/"
        |= namespace
        |. Parser.symbol "/"
        |= (name |> Parser.map makeTopicName)
        |. Parser.end


topicsDecoder : Decode.Decoder (List Topic)
topicsDecoder =
    let
        toTopic : String -> Maybe Topic
        toTopic url =
            url
                |> Parser.run topicParser
                |> Result.toMaybe
    in
    Decode.list Decode.string
        |> Decode.map (List.filterMap toTopic)
