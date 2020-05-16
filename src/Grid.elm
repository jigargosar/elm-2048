module Grid exposing
    ( Entry
    , EntryList
    , Grid
    , Slot(..)
    , emptyPositions
    , fromColumns
    , fromEntries
    , fromRows
    , toColumns
    , toEntries
    , toRows
    )

import Dict
import Dict.Extra as Dict
import IntPos exposing (IntPos)
import IntSize exposing (IntSize)
import PosDict
import Tuple exposing (mapSecond)


type alias Entry a =
    ( IntPos, a )


type alias EntryList a =
    List (Entry a)


type alias PosDict a =
    PosDict.PosDict a


type Slot a
    = Empty
    | Filled a


type Grid a
    = Grid (PosDict (Slot a))


fromEntries : IntSize -> EntryList a -> Grid a
fromEntries s xs =
    PosDict.filled Empty s
        |> PosDict.insertAll (List.map (mapSecond Filled) xs)
        |> Grid


toEntries : Grid a -> EntryList a
toEntries (Grid d) =
    d
        |> Dict.filterMap
            (\_ slot ->
                case slot of
                    Empty ->
                        Nothing

                    Filled a ->
                        Just a
            )
        |> Dict.toList


toRows : Grid a -> List (List (Slot a))
toRows (Grid d) =
    PosDict.toRows d


fromRows : IntSize -> List (List (Slot a)) -> Grid a
fromRows s rs =
    PosDict.fromRows rs
        |> PosDict.resize s Empty
        |> Grid


toColumns : Grid a -> List (List (Slot a))
toColumns (Grid d) =
    PosDict.toColumns d


fromColumns : IntSize -> List (List (Slot a)) -> Grid a
fromColumns s rs =
    PosDict.fromColumns rs
        |> PosDict.resize s Empty
        |> Grid


emptyPositions : Grid a -> List IntPos
emptyPositions (Grid d) =
    d
        |> Dict.filter (\_ v -> v == Empty)
        |> Dict.keys
