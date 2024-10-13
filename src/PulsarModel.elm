module PulsarModel exposing (Mode(..), SubscriptionName, Topic, TopicName, makeSubscriptionName, makeTopicName, subscriptionNameAsString, topicNameAsString)


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


subscriptionNameAsString (SubscriptionName name) =
    name


makeSubscriptionName =
    SubscriptionName


type alias Topic =
    { mode : Mode
    , orga : String
    , namespace : String
    , name : TopicName
    }
