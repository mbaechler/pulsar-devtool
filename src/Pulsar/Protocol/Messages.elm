module Pulsar.Protocol.Messages exposing (Message, messageDecoder)

import Dict
import Http exposing (Metadata)


type alias Message =
    { content : String
    , size : Int
    , sequenceId : Int
    }


messageDecoder : Metadata -> String -> Result String Message
messageDecoder metadata body =
    let
        getIntHeader key =
            Dict.get key metadata.headers
                |> Maybe.andThen String.toInt
    in
    Maybe.map2
        (\size sequenceId -> { content = body, size = size, sequenceId = sequenceId })
        (getIntHeader "x-pulsar-uncompressed-size")
        (getIntHeader "x-pulsar-sequence-id")
        |> Result.fromMaybe "error parsing response"
