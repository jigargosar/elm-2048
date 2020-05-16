module PosDict exposing
    ( PosDict
    , constrain
    , filled
    , fromColumns
    , fromRows
    , insertAll
    , insertEntry
    , mapAccumColumns
    , mapAccumFlippedColumns
    , mapAccumFlippedRows
    , mapAccumRows
    , resize
    , swap
    , toColumns
    , toRows
    )

import Basics.Extra as Basics exposing (uncurry)
import Cons
import Dict exposing (Dict)
import Dict.Extra as Dict
import IntPos exposing (IntPos)
import IntSize exposing (IntSize)
import List.Extra as List
import Tuple exposing (..)


type alias PosDict a =
    Dict IntPos a


type alias Entry a =
    ( IntPos, a )


type alias EntryList a =
    List (Entry a)


fromRows : List (List v) -> PosDict v
fromRows =
    List.indexedMap (\y -> List.indexedMap (\x -> pair ( x, y )))
        >> List.concat
        >> Dict.fromList


resize : IntSize -> a -> PosDict a -> PosDict a
resize s a d =
    Dict.union (constrain s d) (filled a s)


fromColumns : List (List v) -> PosDict v
fromColumns =
    fromRows >> transpose


transpose : PosDict a -> PosDict a
transpose =
    Dict.mapKeys Basics.swap


swap : IntPos -> IntPos -> PosDict a -> PosDict a
swap a b dict =
    dict
        |> Dict.update a (always (Dict.get b dict))
        |> Dict.update b (always (Dict.get a dict))


filled : a -> IntSize -> PosDict a
filled a s =
    IntSize.positions s
        |> List.map (pairTo a)
        |> Dict.fromList


insertEntry : Entry a -> PosDict a -> PosDict a
insertEntry =
    uncurry Dict.insert


insertAll : EntryList a -> PosDict a -> PosDict a
insertAll entryList posDict =
    List.foldl insertEntry posDict entryList


pairTo b a =
    ( a, b )


constrain : IntSize -> PosDict v -> PosDict v
constrain s =
    Dict.filter (\p _ -> IntSize.contains p s)



-- ROWS


mapAccumRows : (a -> List b -> ( a, List c )) -> a -> PosDict b -> ( a, PosDict c )
mapAccumRows reducer acc =
    toRows
        >> List.mapAccuml reducer acc
        >> Tuple.mapSecond fromRows


mapAccumFlippedRows : (a -> List b -> ( a, List c )) -> a -> PosDict b -> ( a, PosDict c )
mapAccumFlippedRows reducer acc =
    toFlippedRows
        >> List.mapAccuml reducer acc
        >> Tuple.mapSecond (fromRows >> flipRows)


toRows : PosDict a -> List (List a)
toRows dict =
    Dict.toList dict
        |> List.gatherEqualsBy (first >> second)
        |> List.map (Cons.toList >> List.map second)


toFlippedRows : PosDict a -> List (List a)
toFlippedRows =
    toRows >> List.map List.reverse


flipRows : PosDict a -> PosDict a
flipRows =
    toFlippedRows >> fromRows



-- COLUMNS


mapAccumColumns : (a -> List b -> ( a, List c )) -> a -> PosDict b -> ( a, PosDict c )
mapAccumColumns reducer acc =
    toColumns
        >> List.mapAccuml reducer acc
        >> Tuple.mapSecond fromColumns


mapAccumFlippedColumns : (a -> List b -> ( a, List c )) -> a -> PosDict b -> ( a, PosDict c )
mapAccumFlippedColumns reducer acc =
    toFlippedColumns
        >> List.mapAccuml reducer acc
        >> Tuple.mapSecond (fromColumns >> flipColumns)


toColumns : PosDict a -> List (List a)
toColumns dict =
    Dict.toList dict
        |> List.gatherEqualsBy (first >> first)
        |> List.map (Cons.toList >> List.map second)


toFlippedColumns : PosDict a -> List (List a)
toFlippedColumns =
    toColumns >> List.map List.reverse


flipColumns : PosDict a -> PosDict a
flipColumns =
    toFlippedColumns >> fromColumns
