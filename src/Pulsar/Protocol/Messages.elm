module Pulsar.Protocol.Messages exposing (Message, messageDecoder)

import Dict
import Http exposing (Metadata)


type alias Message =
    { content : String
    , size : Int
    }


messageDecoder : Metadata -> String -> Result String Message
messageDecoder metadata body =
    Dict.get "x-pulsar-uncompressed-size" metadata.headers
        |> Maybe.andThen String.toInt
        |> Maybe.map (\size -> { content = body, size = size })
        |> Result.fromMaybe "response does not contain message size"
