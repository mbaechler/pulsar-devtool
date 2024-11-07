module ModelTest exposing (suite)

import Dict exposing (Dict)
import Expect
import Fuzz
import PulsarModel exposing (Subscription, makeSubscriptionName)
import String exposing (left)
import Test exposing (describe, fuzz2, test)
import Utils.Grouping exposing (groupByPrefix)


twoStringsWithNoCommonPrefix : Fuzz.Fuzzer ( String.String, String.String )
twoStringsWithNoCommonPrefix =
    someStringsWithNoCommonPrefix 2
        |> Fuzz.andThen
            (\l ->
                case l of
                    [ first, second ] ->
                        Fuzz.constant ( first, second )

                    _ ->
                        Fuzz.invalid "should generate exactly two strings"
            )


stringWithNoCommonPrefix previous =
    let
        string =
            Fuzz.stringOfLengthBetween 1 10

        hasCommonPrefix s =
            previous |> List.any (\p -> String.startsWith (String.left 1 p) s)
    in
    string
        |> Fuzz.andThen
            (\s ->
                if hasCommonPrefix s then
                    stringWithNoCommonPrefix previous

                else
                    Fuzz.constant s
            )


someStringsWithNoCommonPrefix n =
    let
        someStringsWithNoCommonPrefix_ l remaining =
            case remaining of
                1 ->
                    stringWithNoCommonPrefix l
                        |> Fuzz.map (\elm -> elm :: l)

                _ ->
                    stringWithNoCommonPrefix l
                        |> Fuzz.andThen (\elm -> someStringsWithNoCommonPrefix_ (elm :: l) (remaining - 1))
    in
    someStringsWithNoCommonPrefix_ [] n


makeSubscriptionWithName name =
    { name = makeSubscriptionName name
    , durable = True
    , hasConsumers = True
    }


suite =
    describe "grouping subscription"
        [ test "should return empty on empty input" <|
            \_ ->
                groupByPrefix []
                    |> Expect.equal Dict.empty
        , test "should return one subscription" <|
            \_ ->
                groupByPrefix [ makeSubscriptionWithName "lalalala" ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "lalalala", [ makeSubscriptionWithName "lalalala" ] ) ]
                        )
        , test "should return two not-grouped subscriptions" <|
            \_ ->
                groupByPrefix
                    [ makeSubscriptionWithName "lalalala"
                    , makeSubscriptionWithName "fafafa"
                    ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "lalalala", [ makeSubscriptionWithName "lalalala" ] )
                            , ( "fafafa", [ makeSubscriptionWithName "fafafa" ] )
                            ]
                        )
        , fuzz2 (Fuzz.stringOfLengthBetween 1 20) twoStringsWithNoCommonPrefix "should return two grouped subscriptions for any non empty prefix" <|
            \prefix ( left, right ) ->
                groupByPrefix
                    [ makeSubscriptionWithName (prefix ++ left)
                    , makeSubscriptionWithName (prefix ++ right)
                    ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( prefix
                              , [ makeSubscriptionWithName (prefix ++ left)
                                , makeSubscriptionWithName (prefix ++ right)
                                ]
                              )
                            ]
                        )
        , fuzz2 (Fuzz.stringOfLengthBetween 1 20) (someStringsWithNoCommonPrefix 3) "should return 3 grouped subscriptions for any non empty prefix" <|
            \prefix suffixes ->
                let
                    subscriptions =
                        suffixes
                            |> List.map (\suffix -> makeSubscriptionWithName (prefix ++ suffix))
                in
                groupByPrefix subscriptions
                    |> Expect.equal
                        (Dict.fromList
                            [ ( prefix, subscriptions ) ]
                        )
        , test "should return three grouped subscriptions" <|
            \_ ->
                groupByPrefix
                    [ makeSubscriptionWithName "prefix-1"
                    , makeSubscriptionWithName "prefix-2"
                    , makeSubscriptionWithName "prefix-3"
                    ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "prefix-"
                              , [ makeSubscriptionWithName "prefix-1"
                                , makeSubscriptionWithName "prefix-2"
                                , makeSubscriptionWithName "prefix-3"
                                ]
                              )
                            ]
                        )
        , test "should return two grouped subscriptions and one single group" <|
            \_ ->
                groupByPrefix
                    [ makeSubscriptionWithName "prefix-1"
                    , makeSubscriptionWithName "prefix-2"
                    , makeSubscriptionWithName "other"
                    ]
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "prefix-"
                              , [ makeSubscriptionWithName "prefix-1"
                                , makeSubscriptionWithName "prefix-2"
                                ]
                              )
                            , ( "other", [ makeSubscriptionWithName "other" ] )
                            ]
                        )
        ]
