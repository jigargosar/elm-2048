module Board exposing
    ( Board
    , Cell
    , Info
    , Msg(..)
    , decoder
    , encoder
    , generator
    , hasWon
    , info
    , noMovesLeft
    , size
    , update
    )

import Basics.Extra exposing (flip, swap, uncurry)
import Cons exposing (Cons)
import Grid exposing (Grid, Slot(..))
import IncId exposing (IdDict(..), IncId)
import IntPos exposing (IntPos)
import IntSize exposing (IntSize)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDX
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE exposing (Value)
import List.Extra as List
import Random exposing (Generator, Seed(..))
import Random.List
import Tuple exposing (..)



-- BOARD


type Board
    = Board CellGrid


encoder : Board -> Value
encoder (Board cellGrid) =
    JE.object
        [ ( "tag", JE.string "Board" )
        , ( "value", cellGridEncoder cellGrid )
        ]


decoder : Decoder Board
decoder =
    JDX.when (JD.field "tag" JD.string)
        (\tag -> tag == "Board")
        (JD.field "value" cellGridDecoder)
        |> JD.map Board


generator : Generator Board
generator =
    cellGridGenerator
        |> Random.map Board


type alias Info =
    { entries : Grid.EntryList Cell
    , newIds : List IncId
    , newMergedIds : List IncId
    , mergedEntries : Grid.EntryList Cell
    , removedIds : List IncId
    , score : Int
    }


info : Board -> Info
info (Board cellGrid) =
    { entries = IncId.dictValues cellGrid.entriesById
    , newIds = cellGrid.newIds
    , newMergedIds = cellGrid.newMergedIds
    , mergedEntries = cellGrid.mergedEntries
    , removedIds = cellGrid.removedIds
    , score = cellGrid.score
    }


type Msg
    = SlideLeft
    | SlideRight
    | SlideUp
    | SlideDown


hasWon : Board -> Bool
hasWon (Board cellGrid) =
    cellGrid.entriesById
        |> IncId.dictValues
        |> List.any (second >> .num >> (\num -> num >= 2048))


noMovesLeft : Board -> Bool
noMovesLeft board =
    [ SlideLeft, SlideRight, SlideUp, SlideDown ]
        |> List.filterMap (flip update board)
        |> List.length
        |> (==) 0


update : Msg -> Board -> Maybe (Generator Board)
update msg (Board cellGrid) =
    slide msg cellGrid
        |> Maybe.andThen fillRandomEmpty
        |> Maybe.map (Random.map Board)


slide : Msg -> CellGrid -> Maybe CellGrid
slide msg cellGrid =
    let
        acc =
            toGrid cellGrid.entriesById
                |> slideGrid msg
                |> toSlideResponse cellGrid.idSeed

        entriesById =
            IncId.dictFromListBy (second >> .id) acc.entries
    in
    if entriesById == cellGrid.entriesById then
        Nothing

    else
        Just
            { cellGrid
                | idSeed = acc.idSeed
                , entriesById = entriesById
                , newMergedIds = acc.newMergedIds
                , mergedEntries = acc.mergedEntries
                , removedIds = (cellGrid.mergedEntries |> List.map (second >> .id)) ++ cellGrid.removedIds
                , score = cellGrid.score + acc.score
            }



-- CELL


type alias Cell =
    { id : IncId
    , num : Int
    }


cellEncoder : Cell -> Value
cellEncoder cell =
    JE.object <|
        [ ( "id", IncId.encoder cell.id )
        , ( "num", JE.int cell.num )
        ]


cellDecoder : Decoder Cell
cellDecoder =
    JD.succeed Cell
        |> required "id" IncId.decoder
        |> required "num" JD.int


newCell : Int -> IncId.Seed -> ( Cell, IncId.Seed )
newCell num idSeed =
    let
        initCell : IncId -> Cell
        initCell id =
            { id = id, num = num }
    in
    IncId.next idSeed
        |> mapFirst initCell


newCells : List Int -> IncId.Seed -> ( List Cell, IncId.Seed )
newCells numList initialSeed =
    let
        reducer seed num =
            newCell num seed |> swap
    in
    List.mapAccuml reducer initialSeed numList
        |> swap



-- CELL GRID


type alias CellGrid =
    { idSeed : IncId.Seed
    , entriesById : IncId.IdDict (Grid.Entry Cell)
    , mergedEntries : Grid.EntryList Cell
    , removedIds : List IncId
    , newIds : List IncId
    , newMergedIds : List IncId
    , score : Int
    }


cellGridDecoder : Decoder CellGrid
cellGridDecoder =
    JD.succeed CellGrid
        |> required "idSeed" IncId.seedDecoder
        |> required "entriesById" (IncId.dictDecoder cellEntryDecoder)
        |> required "mergedEntries" (JD.list cellEntryDecoder)
        |> required "removedIds" (JD.list IncId.decoder)
        |> required "newIds" (JD.list IncId.decoder)
        |> required "newMergedIds" (JD.list IncId.decoder)
        |> required "score" JD.int


cellGridEncoder : CellGrid -> Value
cellGridEncoder cellGrid =
    JE.object <|
        [ ( "idSeed", IncId.seedEncoder cellGrid.idSeed )
        , ( "entriesById", IncId.dictEncoder cellEntryEncoder cellGrid.entriesById )
        , ( "mergedEntries", JE.list cellEntryEncoder cellGrid.mergedEntries )
        , ( "removedIds", JE.list IncId.encoder cellGrid.removedIds )
        , ( "newIds", JE.list IncId.encoder cellGrid.newIds )
        , ( "newMergedIds", JE.list IncId.encoder cellGrid.newMergedIds )
        , ( "score", JE.int cellGrid.score )
        ]


cellEntryEncoder : Grid.Entry Cell -> Value
cellEntryEncoder ( pos, cell ) =
    JE.object
        [ ( "tag", JE.string "CellEntry" )
        , ( "pos", IntPos.encoder pos )
        , ( "cell", cellEncoder cell )
        ]


cellEntryDecoder : Decoder (Grid.Entry Cell)
cellEntryDecoder =
    JDX.when (JD.field "tag" JD.string)
        (\tag -> tag == "CellEntry")
        (JD.succeed pair
            |> required "pos" IntPos.decoder
            |> required "cell" cellDecoder
        )


size : IntSize
size =
    IntSize.new 4 4


cellGridGenerator : Generator CellGrid
cellGridGenerator =
    initialCellEntriesGenerator
        |> Random.map
            (\( cellEntries, idSeed ) ->
                { idSeed = idSeed
                , entriesById = IncId.dictFromListBy (second >> .id) cellEntries
                , mergedEntries = []
                , removedIds = []
                , newIds = []
                , newMergedIds = []
                , score = 0
                }
            )


numGenerator : Random.Generator Int
numGenerator =
    -- Random.constant 1024
    Random.weighted ( 80, 2 ) [ ( 20, 4 ) ]


initialCellEntriesGenerator : Random.Generator ( Grid.EntryList Cell, IncId.Seed )
initialCellEntriesGenerator =
    let
        initialPositionsGenerator : Random.Generator (List IntPos)
        initialPositionsGenerator =
            Random.List.choose (IntSize.positions size)
                |> Random.andThen
                    (\( p1, ps ) ->
                        Random.List.choose ps
                            |> Random.map
                                (\( p2, _ ) ->
                                    List.filterMap identity [ p1, p2 ]
                                )
                    )
    in
    Random.map2
        (\ps ns ->
            newCells ns IncId.initialSeed
                |> mapFirst (List.zip ps)
        )
        initialPositionsGenerator
        (Random.list 2 numGenerator)


fillRandomEmpty : CellGrid -> Maybe (Generator CellGrid)
fillRandomEmpty cellGrid =
    cellGrid.entriesById
        |> toGrid
        |> Grid.emptyPositions
        |> Cons.fromList
        |> Maybe.map (flip fillRandomEmptyHelp cellGrid)


fillRandomEmptyHelp : Cons IntPos -> CellGrid -> Generator CellGrid
fillRandomEmptyHelp ( h, t ) cellGrid =
    Random.pair
        (Random.uniform h t)
        numGenerator
        |> Random.map
            (\( pos, num ) ->
                let
                    ( cell, idSeed ) =
                        newCell num cellGrid.idSeed
                in
                { cellGrid
                    | entriesById = IncId.dictInsert cell.id ( pos, cell ) cellGrid.entriesById
                    , idSeed = idSeed
                    , newIds = [ cell.id ]
                }
            )



-- SLIDE


type alias SlideResponse =
    { idSeed : IncId.Seed
    , entries : List ( IntPos, Cell )
    , newMergedIds : List IncId
    , mergedEntries : List ( IntPos, Cell )
    , score : Int
    }


toSlideResponse : IncId.Seed -> Grid OutCell -> SlideResponse
toSlideResponse =
    let
        reducer ( pos, slot ) acc =
            case slot of
                Existing cell ->
                    { acc | entries = ( pos, cell ) :: acc.entries }

                Merged c1 c2 ->
                    let
                        ( mergedCell, idSeed ) =
                            newCell (c1.num + c2.num) acc.idSeed
                    in
                    { acc
                        | idSeed = idSeed
                        , entries = ( pos, mergedCell ) :: acc.entries
                        , newMergedIds = mergedCell.id :: acc.newMergedIds
                        , mergedEntries = ( pos, c1 ) :: ( pos, c2 ) :: acc.mergedEntries
                        , score = mergedCell.num + acc.score
                    }
    in
    \idSeed ->
        Grid.toEntries
            >> List.foldl reducer
                { idSeed = idSeed
                , entries = []
                , newMergedIds = []
                , mergedEntries = []
                , score = 0
                }


type OutCell
    = Existing Cell
    | Merged Cell Cell


toGrid : IncId.IdDict (Grid.Entry Cell) -> Grid Cell
toGrid =
    IncId.dictValues
        >> Grid.fromEntries size


slideGrid : Msg -> Grid Cell -> Grid OutCell
slideGrid msg entries =
    case msg of
        SlideLeft ->
            entries
                |> Grid.toRows
                |> List.map (List.reverse >> compactSlotsRight >> List.reverse)
                |> Grid.fromRows size

        SlideRight ->
            entries
                |> Grid.toRows
                |> List.map compactSlotsRight
                |> Grid.fromRows size

        SlideUp ->
            entries
                |> Grid.toColumns
                |> List.map (List.reverse >> compactSlotsRight >> List.reverse)
                |> Grid.fromColumns size

        SlideDown ->
            entries
                |> Grid.toColumns
                |> List.map compactSlotsRight
                |> Grid.fromColumns size


compactSlotsRight : List (Slot Cell) -> List (Slot OutCell)
compactSlotsRight =
    let
        reducer slot ( maybeCell, xs ) =
            case ( slot, maybeCell ) of
                ( Empty, _ ) ->
                    ( maybeCell, xs )

                ( Filled cell, Nothing ) ->
                    ( Just cell, xs )

                ( Filled cell, Just unprocessed ) ->
                    if cell.num == unprocessed.num then
                        ( Nothing, Merged unprocessed cell :: xs )

                    else
                        ( Just cell, Existing unprocessed :: xs )
    in
    List.foldr reducer ( Nothing, [] )
        >> mapFirst (Maybe.map Existing)
        >> uncurry consMaybe
        >> slotsPadLeft size.width


consMaybe : Maybe a -> List a -> List a
consMaybe mx xs =
    case mx of
        Just x ->
            x :: xs

        Nothing ->
            xs


slotsPadLeft : Int -> List a -> List (Slot a)
slotsPadLeft n list =
    let
        len =
            List.length list
    in
    List.repeat (n - len) Empty ++ List.map Filled list
