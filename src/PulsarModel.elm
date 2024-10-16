module PulsarModel exposing (Mode(..), Subscription, SubscriptionName, Topic, TopicName, makeSubscriptionName, makeTopicName, subscriptionNameAsString, topicNameAsString)


type Mode
    = NonPersistent
    | Persistent


type TopicName
    = TopicName String


topicNameAsString (TopicName name) =
    name


makeTopicName =
    TopicName


type SubscriptionName
    = SubscriptionName String


type alias Subscription =
    { name : SubscriptionName
    , durable : Bool
    , hasConsumers : Bool
    }


subscriptionNameAsString : Subscription -> String
subscriptionNameAsString =
    .name >> (\(SubscriptionName s) -> s)


makeSubscriptionName =
    SubscriptionName


type alias Topic =
    { mode : Mode
    , orga : String
    , namespace : String
    , name : TopicName
    }
