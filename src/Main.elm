port module Main exposing (main)

import Board exposing (Board, Msg(..))
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Html.Keyed
import IncId exposing (IncId)
import IntPos exposing (IntPos)
import IntSize
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator, Seed(..))
import Tuple exposing (..)


port cache : String -> Cmd msg



-- Model


type alias Model =
    { status : Status
    , board : Board
    , seed : Random.Seed
    }


encoder : Model -> Value
encoder model =
    JE.object <|
        [ ( "status", statusEncoder model.status )
        , ( "board", Board.encoder model.board )
        ]


decoder : Random.Seed -> Decoder Model
decoder seed =
    JD.succeed Model
        |> required "status" statusDecoder
        |> required "board" Board.decoder
        |> hardcoded seed


type Status
    = Turn
    | NoMoves
    | Won


statusEncoder : Status -> Value
statusEncoder status =
    case status of
        Turn ->
            JE.string "Turn"

        NoMoves ->
            JE.string "NoMoves"

        Won ->
            JE.string "Won"


statusDecoder : Decoder Status
statusDecoder =
    let
        get id =
            case id of
                "Turn" ->
                    JD.succeed Turn

                "NoMoves" ->
                    JD.succeed NoMoves

                "Won" ->
                    JD.succeed Won

                _ ->
                    JD.fail ("unknown value for Status: " ++ id)
    in
    JD.string |> JD.andThen get


type alias Flags =
    { cache : String, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags.now
    in
    case JD.decodeString (decoder initialSeed) flags.cache of
        Ok model ->
            ( model, Cmd.none )

        Err error ->
            let
                _ =
                    Debug.log "error" error

                ( board, seed ) =
                    Random.step Board.generator initialSeed
            in
            ( Model Turn board seed
            , Cmd.none
            )



-- Update


type Msg
    = NoOp
    | OnKeyDown String
    | NewClicked
    | KeepPlayingClicked


updateWrapper : Msg -> Model -> ( Model, Cmd Msg )
updateWrapper message model0 =
    let
        ( newModel, cmd ) =
            update message model0
    in
    ( newModel
    , if newModel /= model0 then
        Cmd.batch [ cmd, cache (encoder newModel |> JE.encode 0) ]

      else
        cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnKeyDown key ->
            case model.status of
                Turn ->
                    case
                        updateBoardFromKey key model.board
                            |> Maybe.map (updateModelFromBoardGenerator model)
                    of
                        Just newModel ->
                            ( newModel, Cmd.none )

                        Nothing ->
                            if Board.noMovesLeft model.board then
                                ( { model | status = NoMoves }, Cmd.none )

                            else
                                ( model, Cmd.none )

                NoMoves ->
                    ( model, Cmd.none )

                Won ->
                    ( model, Cmd.none )

        KeepPlayingClicked ->
            case model.status of
                Turn ->
                    ( model, Cmd.none )

                NoMoves ->
                    ( model, Cmd.none )

                Won ->
                    ( { model | status = Turn }, Cmd.none )

        NewClicked ->
            let
                ( board, seed ) =
                    Random.step Board.generator model.seed
            in
            ( Model Turn board seed, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onKeyDown onKeyDown ]


onKeyDown : Decoder Msg
onKeyDown =
    JD.field "key" JD.string
        |> JD.map OnKeyDown


updateBoardFromKey : String -> Board -> Maybe (Generator Board)
updateBoardFromKey key board =
    case key of
        "ArrowLeft" ->
            Board.update SlideLeft board

        "ArrowRight" ->
            Board.update SlideRight board

        "ArrowUp" ->
            Board.update SlideUp board

        "ArrowDown" ->
            Board.update SlideDown board

        _ ->
            Nothing


updateModelFromBoardGenerator : Model -> Generator Board -> Model
updateModelFromBoardGenerator model generator =
    let
        ( board, seed ) =
            Random.step generator model.seed

        alreadyWon =
            Board.hasWon model.board

        justWon =
            not alreadyWon && Board.hasWon board
    in
    { model
        | board = board
        , status =
            if justWon then
                Won

            else
                Turn
        , seed = seed
    }



-- View


type alias DM =
    Document Msg


type alias HM =
    Html Msg


view : Model -> DM
view model =
    Document "2048 Animated"
        [ div
            [ class "measure center"
            , style "color" "hsl(30, 8%, 43%)"
            ]
            [ viewHeader model.board
            , viewBoard model
            , div [ class "pv3" ]
                [ span [ class "lh-copy" ]
                    [ text "Authored by "
                    , Html.a [ class "color-inherit", href "https://github.com/jigargosar" ] [ text "Jigar Gosar" ]
                    , text ", written in "
                    , Html.a [ class "color-inherit", href "https://elm-lang.org/" ] [ text "elm" ]
                    , text ". "
                    , text "Based on "
                    , Html.a [ class "color-inherit", href "https://play2048.co/" ] [ text "2048" ]
                    , text ", by "
                    , Html.a [ class "color-inherit", href "https://gabrielecirulli.com/" ] [ text "Gabriele Cirulli" ]
                    , text ". "
                    ]
                ]
            ]
        ]


viewHeader : Board -> HM
viewHeader board =
    div [ class "pv3" ]
        [ div [ class "pb2 flex items-center" ]
            [ div [ class "flex-auto pr2" ] [ viewLogo ]
            , div [ class "" ] [ viewScore (Board.info board |> .score) ]
            ]
        , div [ class "flex items-center" ]
            [ div [ class "flex-auto pr2" ] [ viewSubHeading ]
            , div [ class "" ] [ newBtn ]
            ]
        ]


viewLogo =
    Html.strong [ class "b", style "font-size" "4rem" ] [ text "2048" ]


viewSubHeading =
    span [] [ text "Join the numbers and get to the ", Html.strong [] [ text "2048 tile!" ] ]


viewScore score =
    div
        [ class "br2 pa2"
        , class "tc"
        , style "color" "hsl(34, 37%, 96%)"
        , style "background-color" "hsl(29, 17%, 68%)"
        ]
        [ div [ class "ttu" ] [ text "Score" ]
        , div [ class "f3 code b" ] [ text (String.fromInt score) ]
        ]


newBtn =
    btn NewClicked "New Game"


tryAgainBtn =
    btn NewClicked "Try again"


keepGoingBtn =
    btn KeepPlayingClicked "Keep going"


btn msg lbl =
    button
        [ class "bn br2 ma0 pv2 ph3 f4"
        , style "color" "hsl(34, 37%, 96%)"
        , style "background-color" "hsl(29, 17%, 48%)"
        , onClick msg
        ]
        [ text lbl ]


viewBoard : Model -> HM
viewBoard model =
    div
        [ class "code f2 relative center overflow-hidden"
        , style "width" (String.fromInt gridWidth ++ "px")
        , style "height" (String.fromInt gridWidth ++ "px")
        , style "background-color" "hsl(29, 17%, 68%)"
        , style "color" "hsl(30, 8%, 43%)"
        , style "border-radius" "6px"
        ]
        (viewGridBackgroundCells
            :: viewGridCells model.board
            :: (case model.status of
                    Turn ->
                        []

                    NoMoves ->
                        [ overlayContainer "hsla(30, 37%, 89%, 0.73)"
                            [ div [ class "f1 b pv3" ] [ text "Game Over!" ]
                            , div [] [ tryAgainBtn ]
                            ]
                        ]

                    Won ->
                        [ overlayContainer "hsla(46, 84%, 55%, 0.5)"
                            [ div [ class "f1 b pv3" ] [ text "You Win!" ]
                            , div [ class "" ]
                                [ div [ class "pa2" ] [ keepGoingBtn ]
                                , div [ class "pa2" ] [ tryAgainBtn ]
                                ]
                            ]
                        ]
               )
        )


overlayContainer bgc =
    div
        [ class "absolute top-0"
        , class "w-100 h-100"
        , style "background-color" bgc
        , class "pa5"
        , class "tc"
        , class "animate__animated animate__fadeIn animate__slower animate__delay-4s"
        ]


viewGridBackgroundCells : HM
viewGridBackgroundCells =
    div [ class "absolute w-100 h-100" ] (List.map viewCellBackgroundTile (IntSize.positions Board.size))


viewCellBackgroundTile : IntPos -> HM
viewCellBackgroundTile pos =
    div
        [ class "absolute"
        , style "width" (String.fromInt cellWidth ++ "px")
        , style "height" (String.fromInt cellWidth ++ "px")
        , style "transform" (renderTileTransform pos)
        , style "border-radius" "3px"
        , style "background-color" "hsla(30, 37%, 89%, 0.35)"
        ]
        []


viewGridCells : Board -> HM
viewGridCells board =
    Html.Keyed.node "div" [ class "absolute w-100 h-100" ] (viewKeyedCells board)


viewKeyedCells : Board -> List ( String, HM )
viewKeyedCells board =
    let
        { entries, mergedEntries, newIds, newMergedIds, removedIds } =
            Board.info board

        idToAnim : IncId -> TileAnim
        idToAnim id =
            if List.member id newIds then
                Generated

            else if List.member id newMergedIds then
                Merged

            else
                Existing

        cellViewList : List ( IncId, HM )
        cellViewList =
            entries
                |> List.map
                    (\( pos, cell ) ->
                        ( cell.id
                        , viewTile pos cell.num (idToAnim cell.id)
                        )
                    )

        mergedCellViewList : List ( IncId, HM )
        mergedCellViewList =
            List.map
                (\( pos, cell ) ->
                    ( cell.id
                    , viewTile pos cell.num Existing
                    )
                )
                mergedEntries

        removedCellViewList : List ( IncId, HM )
        removedCellViewList =
            List.map (\id -> ( id, text "" )) removedIds
    in
    (cellViewList ++ mergedCellViewList ++ removedCellViewList)
        |> List.sortBy (first >> IncId.toInt)
        |> List.map (mapFirst IncId.toString)



-- Tile


type TileAnim
    = Generated
    | Merged
    | Existing


viewTile : IntPos -> Int -> TileAnim -> HM
viewTile pos num anim =
    div
        [ class "absolute"
        , style "width" (String.fromInt cellWidth ++ "px")
        , style "height" (String.fromInt cellWidth ++ "px")
        , class "flex justify-center items-center"
        , style "transform" (renderTileTransform pos)
        , style "transition" "transform 150ms"
        ]
        [ div
            [ style "background-color" "rgba(255,255,255,0.9)"
            , style "border-radius" "3px"
            , class "w-100 h-100 flex justify-center items-center"
            , style "font-size" (String.fromInt (cellWidth // 2) ++ "px")
            , class "code b"
            , style "color" (numToColor num |> first)
            , style "background-color" (numToColor num |> second)
            , case anim of
                Generated ->
                    class "animate__animated  animate__zoomIn animate__delay-2s "

                Merged ->
                    --class "animate__animated  animate__bounceIn "
                    class "animate__animated  animate__zoomIn "

                Existing ->
                    class ""
            ]
            [ text (String.fromInt num) ]
        ]


numToColor : Int -> ( String, String )
numToColor n =
    case n of
        2 ->
            ( "hsl(30, 8%, 43%)", "hsl(30, 37%, 89%)" )

        4 ->
            ( "hsl(30, 8%, 43%)", "hsl(39, 51%, 86%)" )

        8 ->
            ( "hsl(34, 37%, 96%)", "hsl(28, 82%, 71%)" )

        16 ->
            ( "hsl(34, 37%, 96%)", "hsl(21, 88%, 67%)" )

        32 ->
            ( "hsl(34, 37%, 96%)", "hsl(12, 89%, 67%)" )

        64 ->
            ( "hsl(34, 37%, 96%)", "hsl(11, 91%, 60%)" )

        128 ->
            ( "hsl(34, 37%, 96%)", "hsl(45, 77%, 69%)" )

        256 ->
            ( "hsl(34, 37%, 96%)", "hsl(46, 80%, 65%)" )

        512 ->
            ( "hsl(34, 37%, 96%)", "hsl(46, 81%, 62%)" )

        1024 ->
            ( "hsl(34, 37%, 96%)", "hsl(46, 83%, 59%)" )

        _ ->
            -- 2048
            ( "hsl(34, 37%, 96%)", "hsl(46, 84%, 55%)" )


cellWidth =
    100


cellSpacing =
    15


gridWidth =
    4 * cellWidth + 5 * cellSpacing


renderTileTransform pos =
    let
        postPartToPx n =
            String.fromInt ((n * cellWidth) + ((n + 1) * cellSpacing)) ++ "px"

        ( sx, sy ) =
            pos
                |> mapBoth postPartToPx postPartToPx
    in
    [ "translate(", sx, ",", sy, ")" ]
        |> String.join " "



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = updateWrapper
        , subscriptions = subscriptions
        }
