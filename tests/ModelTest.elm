module ModelTest exposing (suite)

import Dict exposing (Dict)
import Dict.Extra
import Expect
import Fuzz
import List.Extra
import PulsarModel exposing (Subscription, makeSubscriptionName, subscriptionNameAsString)
import String exposing (left)
import Test exposing (describe, fuzz2, fuzz3, skip, test)


type alias Subscriptions2 =
    Dict String (List Subscription)


longestPrefix : String -> String -> String
longestPrefix left right =
    let
        leftL =
            String.toList left

        leftR =
            String.toList right
    in
    List.Extra.zip leftL leftR
        |> List.Extra.takeWhile (\( a, b ) -> a == b)
        |> List.map Tuple.first
        |> String.fromList


cartesianProduct list =
    list
        |> List.concatMap (\v1 -> list |> List.map (\v2 -> ( v1, v2 )))


groupByPrefix : List Subscription -> Subscriptions2
groupByPrefix subscriptions =
    case subscriptions of
        [] ->
            Dict.empty

        [ one ] ->
            Dict.fromList [ ( subscriptionNameAsString one, [ one ] ) ]

        list ->
            let
                subscriptionsByPrefix =
                    list
                        |> cartesianProduct
                        |> List.filter (\( a, b ) -> a /= b)
                        |> Dict.Extra.groupBy (\( a, b ) -> longestPrefix (subscriptionNameAsString a) (subscriptionNameAsString b))
                        |> Dict.map (\p -> \values -> values |> List.concatMap (\( a, b ) -> [ a, b ]) |> List.Extra.unique)
                        |> Dict.toList
                        |> List.sortBy (\( p, _ ) -> String.length p)
                        |> List.reverse

                insert : ( String, List Subscription ) -> Dict String (List Subscription) -> Dict String (List Subscription)
                insert ( p, subscriptions_ ) dict =
                    let
                        alreadyInserted : List Subscription
                        alreadyInserted =
                            dict |> Dict.values |> List.concat
                    in
                    subscriptions_
                        |> List.Extra.filterNot (\s -> List.member s alreadyInserted)
                        |> (\s ->
                                if List.isEmpty s then
                                    dict

                                else
                                    Dict.insert p s dict
                           )

                groups =
                    subscriptionsByPrefix
                        |> List.foldl insert Dict.empty
                        |> Dict.toList
            in
            groups
                |> List.concatMap
                    (\( p, subs ) ->
                        case p of
                            "" ->
                                subs |> List.map (\sub -> ( subscriptionNameAsString sub, [ sub ] ))

                            _ ->
                                [ ( p, subs ) ]
                    )
                |> Dict.fromList


suite =
    describe "grouping subscription"
        [ test "should return empty on empty input" <|
            \_ ->
                groupByPrefix []
                    |> Expect.equal Dict.empty
        , test "should return one subscription" <|
            \_ ->
                let
                    sub1 =
                        { name = makeSubscriptionName "lalalala"
                        , durable = True
                        , hasConsumers = True
                        }
                in
                groupByPrefix [ sub1 ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "lalalala", [ sub1 ] ) ]
                        )
        , test "should return two not-grouped subscriptions" <|
            \_ ->
                let
                    sub1 =
                        { name = makeSubscriptionName "lalalala"
                        , durable = True
                        , hasConsumers = True
                        }

                    sub2 =
                        { name = makeSubscriptionName "fafafa"
                        , durable = True
                        , hasConsumers = True
                        }
                in
                groupByPrefix [ sub1, sub2 ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "lalalala", [ sub1 ] ), ( "fafafa", [ sub2 ] ) ]
                        )

        -- FIXME: left and right should not be generated with a common prefix
        , skip <|
            fuzz3 (Fuzz.stringOfLengthBetween 1 20) Fuzz.string Fuzz.string "should return two grouped subscriptions for any non empty prefix" <|
                \prefix left right ->
                    let
                        sub1 =
                            { name = makeSubscriptionName (prefix ++ left)
                            , durable = True
                            , hasConsumers = True
                            }

                        sub2 =
                            { name = makeSubscriptionName (prefix ++ right)
                            , durable = True
                            , hasConsumers = True
                            }
                    in
                    groupByPrefix [ sub1, sub2 ]
                        |> Expect.equal
                            (Dict.fromList
                                [ ( prefix, [ sub1, sub2 ] ) ]
                            )
        , skip <|
            fuzz2 (Fuzz.stringOfLengthBetween 1 20) (Fuzz.listOfLength 3 Fuzz.string) "should return 3 grouped subscriptions for any non empty prefix" <|
                \prefix suffixes ->
                    let
                        subscriptions =
                            suffixes
                                |> List.map
                                    (\suffix ->
                                        { name = makeSubscriptionName (prefix ++ suffix)
                                        , durable = True
                                        , hasConsumers = True
                                        }
                                    )
                    in
                    groupByPrefix subscriptions
                        |> Expect.equal
                            (Dict.fromList
                                [ ( prefix, subscriptions ) ]
                            )
        , test "should return three grouped subscriptions" <|
            \_ ->
                let
                    sub1 =
                        { name = makeSubscriptionName "prefix-1"
                        , durable = True
                        , hasConsumers = True
                        }

                    sub2 =
                        { name = makeSubscriptionName "prefix-2"
                        , durable = True
                        , hasConsumers = True
                        }

                    sub3 =
                        { name = makeSubscriptionName "prefix-3"
                        , durable = True
                        , hasConsumers = True
                        }
                in
                groupByPrefix [ sub1, sub2, sub3 ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "prefix-", [ sub1, sub2, sub3 ] ) ]
                        )
        , test "should return two grouped subscriptions and one single group" <|
            \_ ->
                let
                    sub1 =
                        { name = makeSubscriptionName "prefix-1"
                        , durable = True
                        , hasConsumers = True
                        }

                    sub2 =
                        { name = makeSubscriptionName "prefix-2"
                        , durable = True
                        , hasConsumers = True
                        }

                    other =
                        { name = makeSubscriptionName "other"
                        , durable = True
                        , hasConsumers = True
                        }
                in
                groupByPrefix [ sub1, sub2, other ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "prefix-", [ sub1, sub2 ] ), ( "other", [ other ] ) ]
                        )
        , skip <|
            test "should return two grouped subscriptions and another group" <|
                \_ ->
                    let
                        prefix =
                            "toto"

                        left =
                            "a"

                        right =
                            "b"

                        other =
                            "lalala"

                        sub1 =
                            { name = makeSubscriptionName (prefix ++ left)
                            , durable = True
                            , hasConsumers = True
                            }

                        sub2 =
                            { name = makeSubscriptionName (prefix ++ right)
                            , durable = True
                            , hasConsumers = True
                            }

                        subother =
                            { name = makeSubscriptionName other
                            , durable = True
                            , hasConsumers = True
                            }
                    in
                    groupByPrefix [ sub1, sub2, subother ]
                        |> Expect.equal
                            (Dict.fromList
                                [ ( prefix, [ sub1, sub2 ] ), ( other, [ subother ] ) ]
                            )
        ]
