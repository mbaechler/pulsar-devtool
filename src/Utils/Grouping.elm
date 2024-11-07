module Utils.Grouping exposing (groupByPrefix)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import PulsarModel exposing (Subscription, subscriptionNameAsString)
import String exposing (left)


type alias GroupedSubscriptions =
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


cartesianProduct : List a -> List ( a, a )
cartesianProduct list =
    list
        |> List.concatMap (\v1 -> list |> List.map (\v2 -> ( v1, v2 )))


groupByPrefix : List Subscription -> GroupedSubscriptions
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
