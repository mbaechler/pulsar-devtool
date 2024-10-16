module Pulsar.Protocol.TopicStats exposing (TopicStats, topicStatsDecoder)

import Json.Decode exposing (succeed)
import Json.Decode.Pipeline exposing (required)


type alias TopicStats =
    { abortedTxnCount : Int
    , averageMsgSize : Float
    , backlogQuotaLimitSize : Int
    , backlogQuotaLimitTime : Int
    , backlogSize : Int
    , bytesInCounter : Int
    , bytesOutCounter : Int
    , bytesOutInternalCounter : Int
    , committedTxnCount : Int
    , compaction : TopicStatsCompaction
    , deduplicationStatus : String
    , delayedMessageIndexSizeInBytes : Int
    , earliestMsgPublishTimeInBacklogs : Int
    , lastOffloadFailureTimeStamp : Int
    , lastOffloadLedgerId : Int
    , lastOffloadSuccessTimeStamp : Int
    , msgChunkPublished : Bool
    , msgInCounter : Int
    , msgOutCounter : Int
    , msgRateIn : Float
    , msgRateOut : Float
    , msgThroughputIn : Float
    , msgThroughputOut : Float
    , nonContiguousDeletedMessagesRanges : Int
    , nonContiguousDeletedMessagesRangesSerializedSize : Int
    , offloadedStorageSize : Int
    , oldestBacklogMessageAgeSeconds : Int
    , ongoingTxnCount : Int
    , ownerBroker : String
    , publishRateLimitedTimes : Int
    , publishers : List TopicStatsPublisher
    , replication : TopicStatsReplication
    , storageSize : Int
    , subscriptions : List TopicStatsSubscription
    , systemTopicBytesInCounter : Int
    , waitingPublishers : Int
    }


type alias TopicStatsCompaction =
    { lastCompactionDurationTimeInMills : Int
    , lastCompactionFailedTimestamp : Int
    , lastCompactionRemovedEventCount : Int
    , lastCompactionSucceedTimestamp : Int
    }


type alias TopicStatsPublisher =
    { accessMode : String
    , address : String
    , averageMsgSize : Float
    , chunkedMessageRate : Float
    , clientVersion : String
    , connectedSince : String
    , metadata : TopicStatsPublisherMetadata
    , msgRateIn : Float
    , msgThroughputIn : Float
    , producerId : Int
    , producerName : String
    , supportsPartialProducer : Bool
    }


type alias TopicStatsPublisherMetadata =
    {}


type alias TopicStatsReplication =
    {}


type alias TopicStatsSubscription =
    { allowOutOfOrderDelivery : Bool
    , backlogSize : Int
    , blockedSubscriptionOnUnackedMsgs : Bool
    , bytesOutCounter : Int
    , chunkedMessageRate : Float
    , consumers : List TopicStatsSubscriptionConsumer
    , consumersAfterMarkDeletePosition : TopicStatsSubscriptionsConsumersAfterMarkDeletePosition
    , delayedMessageIndexSizeInBytes : Int
    , durable : Bool
    , earliestMsgPublishTimeInBacklog : Int
    , filterAcceptedMsgCount : Int
    , filterProcessedMsgCount : Int
    , filterRejectedMsgCount : Int
    , filterRescheduledMsgCount : Int
    , isDurable : Bool
    , isReplicated : Bool
    , lastAckedTimestamp : Int
    , lastConsumedFlowTimestamp : Int
    , lastConsumedTimestamp : Int
    , lastExpireTimestamp : Int
    , lastMarkDeleteAdvancedTimestamp : Int
    , messageAckRate : Float
    , msgBacklog : Int
    , msgBacklogNoDelayed : Int
    , msgDelayed : Int
    , msgInReplay : Int
    , msgOutCounter : Int
    , msgRateExpired : Float
    , msgRateOut : Float
    , msgRateRedeliver : Float
    , msgThroughputOut : Float
    , nonContiguousDeletedMessagesRanges : Int
    , nonContiguousDeletedMessagesRangesSerializedSize : Int
    , replicated : Bool
    , subscriptionProperties : TopicStatsSubscriptionProperties
    , totalMsgExpired : Int
    , type_ : String
    , unackedMessages : Int
    , name : String
    }


type alias TopicStatsSubscriptionConsumer =
    { address : String
    , availablePermits : Int
    , avgMessagesPerEntry : Int
    , blockedConsumerOnUnackedMsgs : Bool
    , bytesOutCounter : Int
    , chunkedMessageRate : Float
    , clientVersion : String
    , connectedSince : String
    , consumerName : String
    , lastAckedTime : String
    , lastAckedTimestamp : Int
    , lastConsumedFlowTimestamp : Int
    , lastConsumedTime : String
    , lastConsumedTimestamp : Int
    , messageAckRate : Float
    , metadata : TopicStatsSubscriptionConsumerMetadata
    , msgOutCounter : Int
    , msgRateOut : Float
    , msgRateRedeliver : Float
    , msgThroughputOut : Float
    , unackedMessages : Int
    }


type alias TopicStatsSubscriptionConsumerMetadata =
    {}


type alias TopicStatsSubscriptionsConsumersAfterMarkDeletePosition =
    {}


type alias TopicStatsSubscriptionProperties =
    {}


topicStatsDecoder : Json.Decode.Decoder TopicStats
topicStatsDecoder =
    succeed TopicStats
        |> required "abortedTxnCount" Json.Decode.int
        |> required "averageMsgSize" Json.Decode.float
        |> required "backlogQuotaLimitSize" Json.Decode.int
        |> required "backlogQuotaLimitTime" Json.Decode.int
        |> required "backlogSize" Json.Decode.int
        |> required "bytesInCounter" Json.Decode.int
        |> required "bytesOutCounter" Json.Decode.int
        |> required "bytesOutInternalCounter" Json.Decode.int
        |> required "committedTxnCount" Json.Decode.int
        |> required "compaction" topicStatsCompactionDecoder
        |> required "deduplicationStatus" Json.Decode.string
        |> required "delayedMessageIndexSizeInBytes" Json.Decode.int
        |> required "earliestMsgPublishTimeInBacklogs" Json.Decode.int
        |> required "lastOffloadFailureTimeStamp" Json.Decode.int
        |> required "lastOffloadLedgerId" Json.Decode.int
        |> required "lastOffloadSuccessTimeStamp" Json.Decode.int
        |> required "msgChunkPublished" Json.Decode.bool
        |> required "msgInCounter" Json.Decode.int
        |> required "msgOutCounter" Json.Decode.int
        |> required "msgRateIn" Json.Decode.float
        |> required "msgRateOut" Json.Decode.float
        |> required "msgThroughputIn" Json.Decode.float
        |> required "msgThroughputOut" Json.Decode.float
        |> required "nonContiguousDeletedMessagesRanges" Json.Decode.int
        |> required "nonContiguousDeletedMessagesRangesSerializedSize" Json.Decode.int
        |> required "offloadedStorageSize" Json.Decode.int
        |> required "oldestBacklogMessageAgeSeconds" Json.Decode.int
        |> required "ongoingTxnCount" Json.Decode.int
        |> required "ownerBroker" Json.Decode.string
        |> required "publishRateLimitedTimes" Json.Decode.int
        |> required "publishers" (Json.Decode.list topicStatsPublisherDecoder)
        |> required "replication" topicStatsReplicationDecoder
        |> required "storageSize" Json.Decode.int
        |> required "subscriptions" topicStatsSubscriptionsDecoder
        |> required "systemTopicBytesInCounter" Json.Decode.int
        |> required "waitingPublishers" Json.Decode.int


topicStatsCompactionDecoder : Json.Decode.Decoder TopicStatsCompaction
topicStatsCompactionDecoder =
    succeed TopicStatsCompaction
        |> required "lastCompactionDurationTimeInMills" Json.Decode.int
        |> required "lastCompactionFailedTimestamp" Json.Decode.int
        |> required "lastCompactionRemovedEventCount" Json.Decode.int
        |> required "lastCompactionSucceedTimestamp" Json.Decode.int


topicStatsPublisherDecoder : Json.Decode.Decoder TopicStatsPublisher
topicStatsPublisherDecoder =
    succeed TopicStatsPublisher
        |> required "accessMode" Json.Decode.string
        |> required "address" Json.Decode.string
        |> required "averageMsgSize" Json.Decode.float
        |> required "chunkedMessageRate" Json.Decode.float
        |> required "clientVersion" Json.Decode.string
        |> required "connectedSince" Json.Decode.string
        |> required "metadata" topicStatsPublisherMetadataDecoder
        |> required "msgRateIn" Json.Decode.float
        |> required "msgThroughputIn" Json.Decode.float
        |> required "producerId" Json.Decode.int
        |> required "producerName" Json.Decode.string
        |> required "supportsPartialProducer" Json.Decode.bool


topicStatsPublisherMetadataDecoder : Json.Decode.Decoder TopicStatsPublisherMetadata
topicStatsPublisherMetadataDecoder =
    succeed TopicStatsPublisherMetadata


topicStatsReplicationDecoder : Json.Decode.Decoder TopicStatsReplication
topicStatsReplicationDecoder =
    succeed TopicStatsReplication


topicStatsSubscriptionsDecoder : Json.Decode.Decoder (List TopicStatsSubscription)
topicStatsSubscriptionsDecoder =
    Json.Decode.keyValuePairs topicStatsSubscriptionDecoder |> Json.Decode.map (List.map (\( name, builder ) -> builder name))


topicStatsSubscriptionDecoder : Json.Decode.Decoder (String -> TopicStatsSubscription)
topicStatsSubscriptionDecoder =
    succeed TopicStatsSubscription
        |> required "allowOutOfOrderDelivery" Json.Decode.bool
        |> required "backlogSize" Json.Decode.int
        |> required "blockedSubscriptionOnUnackedMsgs" Json.Decode.bool
        |> required "bytesOutCounter" Json.Decode.int
        |> required "chunkedMessageRate" Json.Decode.float
        |> required "consumers" (Json.Decode.list topicStatsSubscriptionConsumerDecoder)
        |> required "consumersAfterMarkDeletePosition" topicStatsSubscriptionConsumersAfterMarkDeletePositionDecoder
        |> required "delayedMessageIndexSizeInBytes" Json.Decode.int
        |> required "durable" Json.Decode.bool
        |> required "earliestMsgPublishTimeInBacklog" Json.Decode.int
        |> required "filterAcceptedMsgCount" Json.Decode.int
        |> required "filterProcessedMsgCount" Json.Decode.int
        |> required "filterRejectedMsgCount" Json.Decode.int
        |> required "filterRescheduledMsgCount" Json.Decode.int
        |> required "isDurable" Json.Decode.bool
        |> required "isReplicated" Json.Decode.bool
        |> required "lastAckedTimestamp" Json.Decode.int
        |> required "lastConsumedFlowTimestamp" Json.Decode.int
        |> required "lastConsumedTimestamp" Json.Decode.int
        |> required "lastExpireTimestamp" Json.Decode.int
        |> required "lastMarkDeleteAdvancedTimestamp" Json.Decode.int
        |> required "messageAckRate" Json.Decode.float
        |> required "msgBacklog" Json.Decode.int
        |> required "msgBacklogNoDelayed" Json.Decode.int
        |> required "msgDelayed" Json.Decode.int
        |> required "msgInReplay" Json.Decode.int
        |> required "msgOutCounter" Json.Decode.int
        |> required "msgRateExpired" Json.Decode.float
        |> required "msgRateOut" Json.Decode.float
        |> required "msgRateRedeliver" Json.Decode.float
        |> required "msgThroughputOut" Json.Decode.float
        |> required "nonContiguousDeletedMessagesRanges" Json.Decode.int
        |> required "nonContiguousDeletedMessagesRangesSerializedSize" Json.Decode.int
        |> required "replicated" Json.Decode.bool
        |> required "subscriptionProperties" topicStatsSubscriptionPropertiesDecoder
        |> required "totalMsgExpired" Json.Decode.int
        |> required "type" Json.Decode.string
        |> required "unackedMessages" Json.Decode.int


topicStatsSubscriptionConsumerDecoder : Json.Decode.Decoder TopicStatsSubscriptionConsumer
topicStatsSubscriptionConsumerDecoder =
    succeed TopicStatsSubscriptionConsumer
        |> required "address" Json.Decode.string
        |> required "availablePermits" Json.Decode.int
        |> required "avgMessagesPerEntry" Json.Decode.int
        |> required "blockedConsumerOnUnackedMsgs" Json.Decode.bool
        |> required "bytesOutCounter" Json.Decode.int
        |> required "chunkedMessageRate" Json.Decode.float
        |> required "clientVersion" Json.Decode.string
        |> required "connectedSince" Json.Decode.string
        |> required "consumerName" Json.Decode.string
        |> required "lastAckedTime" Json.Decode.string
        |> required "lastAckedTimestamp" Json.Decode.int
        |> required "lastConsumedFlowTimestamp" Json.Decode.int
        |> required "lastConsumedTime" Json.Decode.string
        |> required "lastConsumedTimestamp" Json.Decode.int
        |> required "messageAckRate" Json.Decode.float
        |> required "metadata" topicStatsSubscriptionConsumerMetadataDecoder
        |> required "msgOutCounter" Json.Decode.int
        |> required "msgRateOut" Json.Decode.float
        |> required "msgRateRedeliver" Json.Decode.float
        |> required "msgThroughputOut" Json.Decode.float
        |> required "unackedMessages" Json.Decode.int


topicStatsSubscriptionConsumerMetadataDecoder : Json.Decode.Decoder TopicStatsSubscriptionConsumerMetadata
topicStatsSubscriptionConsumerMetadataDecoder =
    succeed TopicStatsSubscriptionConsumerMetadata


topicStatsSubscriptionConsumersAfterMarkDeletePositionDecoder : Json.Decode.Decoder TopicStatsSubscriptionsConsumersAfterMarkDeletePosition
topicStatsSubscriptionConsumersAfterMarkDeletePositionDecoder =
    succeed TopicStatsSubscriptionsConsumersAfterMarkDeletePosition


topicStatsSubscriptionPropertiesDecoder : Json.Decode.Decoder TopicStatsSubscriptionProperties
topicStatsSubscriptionPropertiesDecoder =
    succeed TopicStatsSubscriptionProperties
