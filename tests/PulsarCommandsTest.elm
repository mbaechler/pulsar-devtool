module PulsarCommandsTest exposing (suite)

import Expect exposing (Expectation)
import Json.Decode
import PulsarCommands exposing (internalInfoDecoder, topicsDecoder)
import PulsarModel exposing (Mode(..))
import PulsarCommands exposing (topicsDecoder)
import Test exposing (..)
import Time exposing (millisToPosix)


suite =
    describe "list topics decoder"
        [ test "should decode a non-empty list" <|
            \_ ->
                let
                    json =
                        """
                        [ "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/James-outgoing-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/James-spool-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/outgoing-scheduled-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/pmq-filter-outgoing-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/pmq-filter-scheduled-outgoing-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/pmq-filter-scheduled-spool-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/pmq-filter-spool-partition-0"
                        , "persistent://orga_e8400f27-5fb2-4a31-933e-539e63507291/pulsar_18866379-bc72-4180-9a01-207bae3ee6c3/spool-scheduled-partition-0"
                        ]"""
                in
                Json.Decode.decodeString topicsDecoder json
                    |> Expect.equal
                        (Ok
                            [ { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "James-outgoing-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "James-spool-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "outgoing-scheduled-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "pmq-filter-outgoing-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "pmq-filter-scheduled-outgoing-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "pmq-filter-scheduled-spool-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "pmq-filter-spool-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = "spool-scheduled-partition-0" }
                            ]
                        )
        , test "should decode internal info" <|
            \_ ->
                let
                    json =
                        """{
                                "version": 121,
                                "creationDate": "2023-09-21T20:42:59.907Z",
                                "modificationDate": "2024-10-07T10:24:13.774Z",
                                "ledgers": [
                                  {
                                    "ledgerId": 103151222,
                                    "entries": 4,
                                    "size": 4432,
                                    "timestamp": 1695343591529,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 149914877,
                                    "timestamp": 1728463452466,
                                    "isOffloaded": false
                                  }
                                ],
                                "cursors": {
                                  "subscription-outgoing": {
                                    "version": 160,
                                    "creationDate": "2023-09-21T20:43:00.662Z",
                                    "modificationDate": "2024-10-02T14:51:26.821Z",
                                    "cursorsLedgerId": -1,
                                    "markDelete": {
                                      "ledgerId": 147306991,
                                      "entryId": 0
                                    }
                                  }
                                }
                              }"""
                in
                Json.Decode.decodeString internalInfoDecoder json
                    |> Expect.equal
                        (Ok
                            { version = 121
                            , creationDate = "2023-09-21T20:42:59.907Z"
                            , modificationDate = "2024-10-07T10:24:13.774Z"
                            , ledgers =
                                [ { ledgerId = 103151222
                                  , entries = Just 4
                                  , size = Just 4432
                                  , timestamp = millisToPosix 1695343591529
                                  , isOffloaded = False
                                  }
                                , { ledgerId = 149914877
                                  , entries = Nothing
                                  , size = Nothing
                                  , timestamp = millisToPosix 1728463452466
                                  , isOffloaded = False
                                  }
                                ]
                            }
                        )
        ]
