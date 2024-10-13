module Secret exposing (Msg(..), loadPulsarToken, savePulsarToken, subscriptions)

import Json.Decode exposing (Error)
import Json.Encode
import LocalStorage
import Ports.LocalStorage as LocalStoragePort exposing (clear, getItem, listKeys, setItem)
import Pulsar exposing (PulsarToken)


localstorage =
    LocalStorage.make getItem setItem clear listKeys "pulsar-devtool"


type Msg
    = TokenLoadSuccess (Maybe PulsarToken)
    | TokenLoadFailure String
    | UnexpectedEvent String


loadPulsarToken : Cmd Msg
loadPulsarToken =
    LocalStorage.getItem localstorage "token"


subscriptions : Sub Msg
subscriptions =
    LocalStorage.responseHandler mapResponse localstorage
        |> LocalStoragePort.response


mapResponse : LocalStorage.Response -> Msg
mapResponse response =
    case response of
        LocalStorage.Item "token" json ->
            case decodeToken json of
                Err message ->
                    TokenLoadFailure <| Json.Decode.errorToString message

                Ok token ->
                    TokenLoadSuccess <| Just <| Pulsar.makeToken token

        LocalStorage.Item key _ ->
            UnexpectedEvent <| "item loaded with key " ++ key

        LocalStorage.ItemNotFound "token" ->
            TokenLoadSuccess <| Nothing

        LocalStorage.ItemNotFound key ->
            UnexpectedEvent <| "item not found with key " ++ key

        LocalStorage.KeyList _ ->
            UnexpectedEvent <| "subscription message for keylist"

        LocalStorage.Error message ->
            UnexpectedEvent <| "error : " ++ message


encodeToken : PulsarToken -> Json.Encode.Value
encodeToken pulsarToken =
    pulsarToken
        |> Pulsar.tokenAsString
        |> Json.Encode.string


decodeToken : Json.Encode.Value -> Result Error String
decodeToken value =
    Json.Decode.decodeValue Json.Decode.string value


savePulsarToken : PulsarToken -> Cmd msg
savePulsarToken token =
    LocalStorage.setItem localstorage "token" (encodeToken token)
