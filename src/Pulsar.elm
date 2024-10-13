module Pulsar exposing (PulsarToken, makeToken, tokenAsString)


type PulsarToken
    = PulsarToken String


makeToken =
    PulsarToken


tokenAsString (PulsarToken secret) =
    secret
