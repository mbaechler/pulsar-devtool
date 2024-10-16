module Pulsar.Protocol.TopicInternalInfo exposing (InternalInfo, internalInfoDecoder)

import Json.Decode as Decode exposing (Error)
import Json.Decode.Pipeline exposing (optional, required)
import Time exposing (Posix, millisToPosix)


type alias InternalInfo =
    { version : Int
    , creationDate : String
    , modificationDate : String
    , ledgers : List Ledger
    }


type alias Ledger =
    { ledgerId : Int
    , entries : Maybe Int
    , size : Maybe Int
    , timestamp : Posix
    , isOffloaded : Bool
    }


internalInfoDecoder : Decode.Decoder InternalInfo
internalInfoDecoder =
    Decode.succeed InternalInfo
        |> required "version" Decode.int
        |> required "creationDate" Decode.string
        |> required "modificationDate" Decode.string
        |> required "ledgers" (Decode.list ledgerDecoder)


ledgerDecoder : Decode.Decoder Ledger
ledgerDecoder =
    Decode.succeed Ledger
        |> required "ledgerId" Decode.int
        |> optional "entries" (Decode.nullable Decode.int) Nothing
        |> optional "size" (Decode.nullable Decode.int) Nothing
        |> required "timestamp" (Decode.int |> Decode.map millisToPosix)
        |> required "isOffloaded" Decode.bool
