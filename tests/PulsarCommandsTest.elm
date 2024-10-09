module PulsarCommandsTest exposing (suite)

import Expect exposing (Expectation)
import Json.Decode
import PulsarCommands exposing (internalInfoDecoder, topicsDecoder)
import PulsarModel exposing (Mode(..))
import PulsarCommands exposing (topicsDecoder)
import Test exposing (..)


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
                                    "ledgerId": 108855190,
                                    "entries": 1,
                                    "size": 1272,
                                    "timestamp": 1701984360756,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 109237456,
                                    "entries": 20,
                                    "size": 26046,
                                    "timestamp": 1702000634235,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 114790522,
                                    "entries": 1,
                                    "size": 1098,
                                    "timestamp": 1708004774689,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 124262712,
                                    "entries": 1,
                                    "size": 1100,
                                    "timestamp": 1713911038895,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 133231617,
                                    "entries": 1,
                                    "size": 1100,
                                    "timestamp": 1720430830044,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 135898999,
                                    "entries": 1,
                                    "size": 919,
                                    "timestamp": 1721664777871,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 136565308,
                                    "entries": 6,
                                    "size": 9425,
                                    "timestamp": 1721679535236,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 136594511,
                                    "entries": 1,
                                    "size": 1115,
                                    "timestamp": 1721719975636,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 136669640,
                                    "entries": 6,
                                    "size": 20364,
                                    "timestamp": 1721734733001,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 136697823,
                                    "entries": 1,
                                    "size": 1275,
                                    "timestamp": 1721822139572,
                                    "isOffloaded": false
                                  },
                                  {
                                    "ledgerId": 147306991,
                                    "entries": 1,
                                    "size": 1301,
                                    "timestamp": 1727730819868,
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
                        (Ok { version = 121, creationDate = "2023-09-21T20:42:59.907Z", modificationDate = "2024-10-07T10:24:13.774Z" })
        ]
