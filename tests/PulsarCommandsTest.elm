module PulsarCommandsTest exposing (suite)

import Expect exposing (Expectation)
import Json.Decode
import Pulsar.Protocol.TopicInternalInfo exposing (internalInfoDecoder)
import Pulsar.Protocol.TopicStats exposing (topicStatsDecoder)
import Pulsar.Protocol.Topics exposing (topicsDecoder)
import PulsarModel exposing (Mode(..), makeSubscriptionName, makeTopicName)
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
                            [ { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "James-outgoing-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "James-spool-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "outgoing-scheduled-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "pmq-filter-outgoing-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "pmq-filter-scheduled-outgoing-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "pmq-filter-scheduled-spool-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "pmq-filter-spool-partition-0" }
                            , { mode = Persistent, orga = "orga_e8400f27-5fb2-4a31-933e-539e63507291", namespace = "pulsar_18866379-bc72-4180-9a01-207bae3ee6c3", name = makeTopicName "spool-scheduled-partition-0" }
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
        , test "should decode stats" <|
            \_ ->
                let
                    json =
                        """{
                             "msgRateIn": 0.0,
                             "msgThroughputIn": 0.0,
                             "msgRateOut": 0.0,
                             "msgThroughputOut": 0.0,
                             "bytesInCounter": 1301,
                             "msgInCounter": 1,
                             "systemTopicBytesInCounter": 0,
                             "bytesOutCounter": 1301,
                             "msgOutCounter": 1,
                             "bytesOutInternalCounter": 0,
                             "averageMsgSize": 0.0,
                             "msgChunkPublished": false,
                             "storageSize": 70748,
                             "backlogSize": 0,
                             "backlogQuotaLimitSize": -1,
                             "backlogQuotaLimitTime": -1,
                             "oldestBacklogMessageAgeSeconds": 0,
                             "publishRateLimitedTimes": 0,
                             "earliestMsgPublishTimeInBacklogs": 0,
                             "offloadedStorageSize": 0,
                             "lastOffloadLedgerId": 0,
                             "lastOffloadSuccessTimeStamp": 0,
                             "lastOffloadFailureTimeStamp": 0,
                             "ongoingTxnCount": 0,
                             "abortedTxnCount": 0,
                             "committedTxnCount": 0,
                             "publishers": [
                               {
                                 "accessMode": "Shared",
                                 "msgRateIn": 0.0,
                                 "msgThroughputIn": 0.0,
                                 "averageMsgSize": 0.0,
                                 "chunkedMessageRate": 0.0,
                                 "producerId": 4,
                                 "supportsPartialProducer": false,
                                 "producerName": "c3-813-55563",
                                 "address": "/192.168.1.5:48306",
                                 "connectedSince": "2024-10-16T06:58:14.750976682Z",
                                 "clientVersion": "2.11.0",
                                 "metadata": {}
                               }
                             ],
                             "waitingPublishers": 0,
                             "subscriptions": {
                               "subscription-outgoing": {
                                 "msgRateOut": 0.0,
                                 "msgThroughputOut": 0.0,
                                 "bytesOutCounter": 1301,
                                 "msgOutCounter": 1,
                                 "msgRateRedeliver": 0.0,
                                 "messageAckRate": 0.0,
                                 "chunkedMessageRate": 0.0,
                                 "msgBacklog": 0,
                                 "backlogSize": 0,
                                 "earliestMsgPublishTimeInBacklog": 0,
                                 "msgBacklogNoDelayed": 0,
                                 "blockedSubscriptionOnUnackedMsgs": false,
                                 "msgDelayed": 0,
                                 "msgInReplay": 0,
                                 "unackedMessages": 0,
                                 "type": "Shared",
                                 "msgRateExpired": 0.0,
                                 "totalMsgExpired": 0,
                                 "lastExpireTimestamp": 0,
                                 "lastConsumedFlowTimestamp": 1729061895242,
                                 "lastConsumedTimestamp": 0,
                                 "lastAckedTimestamp": 0,
                                 "lastMarkDeleteAdvancedTimestamp": 1728482150310,
                                 "consumers": [
                                   {
                                     "msgRateOut": 0.0,
                                     "msgThroughputOut": 0.0,
                                     "bytesOutCounter": 0,
                                     "msgOutCounter": 0,
                                     "msgRateRedeliver": 0.0,
                                     "messageAckRate": 0.0,
                                     "chunkedMessageRate": 0.0,
                                     "consumerName": "f9e25",
                                     "availablePermits": 1000,
                                     "unackedMessages": 0,
                                     "avgMessagesPerEntry": 0,
                                     "blockedConsumerOnUnackedMsgs": false,
                                     "address": "/192.168.1.5:48306",
                                     "connectedSince": "2024-10-16T06:58:15.23820835Z",
                                     "clientVersion": "2.11.0",
                                     "lastAckedTimestamp": 0,
                                     "lastConsumedTimestamp": 0,
                                     "lastConsumedFlowTimestamp": 1729061895242,
                                     "metadata": {},
                                     "lastAckedTime": "1970-01-01T00:00:00Z",
                                     "lastConsumedTime": "1970-01-01T00:00:00Z"
                                   }
                                 ],
                                 "isDurable": true,
                                 "isReplicated": false,
                                 "allowOutOfOrderDelivery": false,
                                 "consumersAfterMarkDeletePosition": {},
                                 "nonContiguousDeletedMessagesRanges": 0,
                                 "nonContiguousDeletedMessagesRangesSerializedSize": 0,
                                 "delayedMessageIndexSizeInBytes": 0,
                                 "subscriptionProperties": {},
                                 "filterProcessedMsgCount": 0,
                                 "filterAcceptedMsgCount": 0,
                                 "filterRejectedMsgCount": 0,
                                 "filterRescheduledMsgCount": 0,
                                 "durable": true,
                                 "replicated": false
                               }
                             },
                             "replication": {},
                             "deduplicationStatus": "Disabled",
                             "nonContiguousDeletedMessagesRanges": 0,
                             "nonContiguousDeletedMessagesRangesSerializedSize": 0,
                             "delayedMessageIndexSizeInBytes": 0,
                             "compaction": {
                               "lastCompactionRemovedEventCount": 0,
                               "lastCompactionSucceedTimestamp": 0,
                               "lastCompactionFailedTimestamp": 0,
                               "lastCompactionDurationTimeInMills": 0
                             },
                             "ownerBroker": "clevercloud-pulsar-broker-c3-n3:8080"
                           }"""
                in
                Json.Decode.decodeString topicStatsDecoder json
                    |> Expect.equal
                        (Ok
                            { msgRateIn = 0.0
                            , msgThroughputIn = 0.0
                            , msgRateOut = 0.0
                            , msgThroughputOut = 0.0
                            , bytesInCounter = 1301
                            , msgInCounter = 1
                            , systemTopicBytesInCounter = 0
                            , bytesOutCounter = 1301
                            , msgOutCounter = 1
                            , bytesOutInternalCounter = 0
                            , averageMsgSize = 0.0
                            , msgChunkPublished = False
                            , storageSize = 70748
                            , backlogSize = 0
                            , backlogQuotaLimitSize = -1
                            , backlogQuotaLimitTime = -1
                            , oldestBacklogMessageAgeSeconds = 0
                            , publishRateLimitedTimes = 0
                            , earliestMsgPublishTimeInBacklogs = 0
                            , offloadedStorageSize = 0
                            , lastOffloadLedgerId = 0
                            , lastOffloadSuccessTimeStamp = 0
                            , lastOffloadFailureTimeStamp = 0
                            , ongoingTxnCount = 0
                            , abortedTxnCount = 0
                            , committedTxnCount = 0
                            , publishers =
                                [ { accessMode = "Shared"
                                  , msgRateIn = 0.0
                                  , msgThroughputIn = 0.0
                                  , averageMsgSize = 0.0
                                  , chunkedMessageRate = 0.0
                                  , producerId = 4
                                  , supportsPartialProducer = False
                                  , producerName = "c3-813-55563"
                                  , address = "/192.168.1.5:48306"
                                  , connectedSince = "2024-10-16T06:58:14.750976682Z"
                                  , clientVersion = "2.11.0"
                                  , metadata = {}
                                  }
                                ]
                            , waitingPublishers = 0
                            , subscriptions =
                                [ { name = makeSubscriptionName "subscription-outgoing"
                                  , msgRateOut = 0.0
                                  , msgThroughputOut = 0.0
                                  , bytesOutCounter = 1301
                                  , msgOutCounter = 1
                                  , msgRateRedeliver = 0.0
                                  , messageAckRate = 0.0
                                  , chunkedMessageRate = 0.0
                                  , msgBacklog = 0
                                  , backlogSize = 0
                                  , earliestMsgPublishTimeInBacklog = 0
                                  , msgBacklogNoDelayed = 0
                                  , blockedSubscriptionOnUnackedMsgs = False
                                  , msgDelayed = 0
                                  , msgInReplay = 0
                                  , unackedMessages = 0
                                  , type_ = "Shared"
                                  , msgRateExpired = 0.0
                                  , totalMsgExpired = 0
                                  , lastExpireTimestamp = 0
                                  , lastConsumedFlowTimestamp = 1729061895242
                                  , lastConsumedTimestamp = 0
                                  , lastAckedTimestamp = 0
                                  , lastMarkDeleteAdvancedTimestamp = 1728482150310
                                  , consumers =
                                        [ { msgRateOut = 0.0
                                          , msgThroughputOut = 0.0
                                          , bytesOutCounter = 0
                                          , msgOutCounter = 0
                                          , msgRateRedeliver = 0.0
                                          , messageAckRate = 0.0
                                          , chunkedMessageRate = 0.0
                                          , consumerName = "f9e25"
                                          , availablePermits = 1000
                                          , unackedMessages = 0
                                          , avgMessagesPerEntry = 0
                                          , blockedConsumerOnUnackedMsgs = False
                                          , address = "/192.168.1.5:48306"
                                          , connectedSince = "2024-10-16T06:58:15.23820835Z"
                                          , clientVersion = "2.11.0"
                                          , lastAckedTimestamp = 0
                                          , lastConsumedTimestamp = 0
                                          , lastConsumedFlowTimestamp = 1729061895242
                                          , metadata = {}
                                          , lastAckedTime = "1970-01-01T00:00:00Z"
                                          , lastConsumedTime = "1970-01-01T00:00:00Z"
                                          }
                                        ]
                                  , allowOutOfOrderDelivery = False
                                  , consumersAfterMarkDeletePosition = {}
                                  , nonContiguousDeletedMessagesRanges = 0
                                  , nonContiguousDeletedMessagesRangesSerializedSize = 0
                                  , delayedMessageIndexSizeInBytes = 0
                                  , subscriptionProperties = {}
                                  , filterProcessedMsgCount = 0
                                  , filterAcceptedMsgCount = 0
                                  , filterRejectedMsgCount = 0
                                  , filterRescheduledMsgCount = 0
                                  , durable = True
                                  , replicated = False
                                  }
                                ]
                            , replication = {}
                            , deduplicationStatus = "Disabled"
                            , nonContiguousDeletedMessagesRanges = 0
                            , nonContiguousDeletedMessagesRangesSerializedSize = 0
                            , delayedMessageIndexSizeInBytes = 0
                            , compaction =
                                { lastCompactionRemovedEventCount = 0
                                , lastCompactionSucceedTimestamp = 0
                                , lastCompactionFailedTimestamp = 0
                                , lastCompactionDurationTimeInMills = 0
                                }
                            , ownerBroker = "clevercloud-pulsar-broker-c3-n3:8080"
                            }
                        )
        ]
