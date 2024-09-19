module PulsarCommandsTest exposing (suite)

import Expect exposing (Expectation)
import Json.Decode
import PulsarCommands exposing (Mode(..), topicsDecoder)
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
        ]
