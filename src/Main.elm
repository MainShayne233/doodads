module Main exposing (..)

import Time exposing (Time, every, millisecond)
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Extra exposing (Key)
import AnimationFrame


---- MODEL ----


type alias Model =
    { pieceInstances : List PieceInstance
    , timeSinceLastMove : Time
    , timePerMove : Time
    }


initPieceInstances : List PieceInstance
initPieceInstances =
    [ { pieceGrid = triGuyGrid, coordinate = ( 13, 2 ), color = Green }
    ]


init : ( Model, Cmd Msg )
init =
    ( { pieceInstances = initPieceInstances
      , timeSinceLastMove = 0
      , timePerMove = 1000
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Change String
    | TimeUpdate Time
    | KeyDown Key
    | Move
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                updatedModel =
                    handleKeyPress key model
            in
                ( updatedModel, Cmd.none )

        TimeUpdate time ->
            let
                { timeSinceLastMove, timePerMove, pieceInstances } =
                    model

                newTimeSinceLastMove =
                    timeSinceLastMove + time

                ( updatedTimeSinceLastMove, newPieceInstances ) =
                    if newTimeSinceLastMove > timePerMove then
                        ( 0, executeMove pieceInstances )
                    else
                        ( newTimeSinceLastMove, pieceInstances )

                updatedPieceInstances =
                    detectCollisions newPieceInstances
            in
                ( { model
                    | timeSinceLastMove = updatedTimeSinceLastMove
                    , pieceInstances = updatedPieceInstances
                  }
                , Cmd.none
                )

        other ->
            ( model, Cmd.none )


detectCollisions : List PieceInstance -> List PieceInstance
detectCollisions pieceInstances =
    pieceInstances



--    case pieceInstances of
--        [] ->
--            []
--
--        activePieceInstance :: restOfPieceInstances ->
--            let
--                isCollision =
--                    checkForCollisions activePieceInstance.pieceGrid restOfPieceInstances
--            in
--                case isCollision of
--                    True ->
--                        { pieceSpec = pieceSpec FourSquare, coordinate = ( 13, 1 ) } :: activePiece :: restOfPieces
--
--                    False ->
--                        activePiece :: restOfPieces
--checkForCollisions : Grid -> List PieceInstance -> Bool
--checkForCollisions grid pieceInstances =
--    if (List.any coordianteTouchesFloor grid) then
--        True
--    else
--        List.any (piecesCollide grid) pieceInstances
--piecesCollide : Grid -> PieceInstance -> Bool
--piecesCollide grid { pieceSpec } =
--    pieceSpec.layout
--        |> layoutToGrid
--        |> List.any (hasCoordinate grid)
--
--
--hasCoordinate : Grid -> Coordinate -> Bool
--hasCoordinate grid coordinate =
--    List.member coordinate grid
--
--
--coordianteTouchesFloor : Coordinate -> Bool
--coordianteTouchesFloor ( xCoordinate, yCoordinate ) =
--    yCoordinate >= 10
--
--
--containsCoordinate : Grid -> Coordinate -> Bool
--containsCoordinate grid coordinate =
--    List.member coordinate grid


executeMove : List PieceInstance -> List PieceInstance
executeMove pieceInstances =
    case pieceInstances of
        [] ->
            []

        activePiece :: restOfPieces ->
            let
                ( xCoordinate, yCoordinate ) =
                    activePiece.coordinate

                updatedYCoordinate =
                    yCoordinate + 1

                updatedActivePiece =
                    { activePiece | coordinate = ( xCoordinate, updatedYCoordinate ) }
            in
                updatedActivePiece :: restOfPieces


type MoveDirection
    = Right
    | Left
    | Down


movePieceInstance : MoveDirection -> PieceInstance -> PieceInstance
movePieceInstance direction pieceInstance =
    let
        ( xCoordinate, yCoordinate ) =
            pieceInstance.coordinate

        updatedCoordinate =
            case direction of
                Right ->
                    ( xCoordinate + 1, yCoordinate )

                Left ->
                    ( xCoordinate - 1, yCoordinate )

                Down ->
                    ( xCoordinate, yCoordinate + 1 )
    in
        { pieceInstance | coordinate = updatedCoordinate }


handleKeyPress : Key -> Model -> Model
handleKeyPress key model =
    case model.pieceInstances of
        [] ->
            model

        activePieceInstance :: inactivePieceInstances ->
            case key of
                Keyboard.Extra.CharA ->
                    let
                        movedPieceInstance =
                            movePieceInstance Left activePieceInstance
                    in
                        { model | pieceInstances = movedPieceInstance :: inactivePieceInstances }

                Keyboard.Extra.CharD ->
                    let
                        movedPieceInstance =
                            movePieceInstance Right activePieceInstance
                    in
                        { model | pieceInstances = movedPieceInstance :: inactivePieceInstances }

                Keyboard.Extra.CharS ->
                    let
                        movedPieceInstance =
                            movePieceInstance Down activePieceInstance
                    in
                        { model | pieceInstances = movedPieceInstance :: inactivePieceInstances }

                other_ ->
                    model



---- VIEW ----


view : Model -> Html Msg
view { pieceInstances } =
    div []
        [ h1 []
            [ text "Doodads" ]
        , div
            []
            (renderPieceInstances pieceInstances)
        ]


renderPieceInstances : List PieceInstance -> List (Html Msg)
renderPieceInstances instances =
    List.map renderPieceInstance instances


type Piece
    = FourSquare
    | LongOne
    | LeftNoodle
    | RightNoodle
    | LeftHook
    | RightHook
    | TriGuy



--    | LeftNoodle
--    | RightNoodle
--    | LeftHook
--    | RightHook
--    | TriGuy


type alias PieceInstance =
    { pieceGrid : PieceGrid, coordinate : Coordinate, color : Color }


type Color
    = Yellow
    | Blue
    | Red
    | Green
    | Purple
    | Cyan
    | Orange


type alias PieceSpec =
    { layout : PieceLayout
    , color : Color
    }


type CellState
    = Filled
    | NotFilled


type alias PieceRow =
    ( CellState, CellState, CellState, CellState )


type alias PieceGrid =
    ( PieceRow, PieceRow, PieceRow, PieceRow )


type alias Row =
    List CellState


type alias PieceLayout =
    List Row


type alias Coordinate =
    ( Int, Int )


type alias Grid =
    List Coordinate


fourSquareGrid : PieceGrid
fourSquareGrid =
    ( ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, Filled, Filled, NotFilled )
    , ( NotFilled, Filled, Filled, NotFilled )
    , ( NotFilled, NotFilled, NotFilled, NotFilled )
    )


longOneGrid : PieceGrid
longOneGrid =
    ( ( NotFilled, Filled, NotFilled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    )


leftNoodleGrid : PieceGrid
leftNoodleGrid =
    ( ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    , ( NotFilled, Filled, Filled, NotFilled )
    , ( NotFilled, NotFilled, Filled, NotFilled )
    )


rightNoodleGrid : PieceGrid
rightNoodleGrid =
    ( ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, NotFilled, Filled, NotFilled )
    , ( NotFilled, Filled, Filled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    )


leftHookGrid : PieceGrid
leftHookGrid =
    ( ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, Filled, Filled, NotFilled )
    , ( NotFilled, NotFilled, Filled, NotFilled )
    , ( NotFilled, NotFilled, Filled, NotFilled )
    )


rightHookGrid : PieceGrid
rightHookGrid =
    ( ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, Filled, Filled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    , ( NotFilled, Filled, NotFilled, NotFilled )
    )


triGuyGrid : PieceGrid
triGuyGrid =
    ( ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, NotFilled, NotFilled, NotFilled )
    , ( NotFilled, NotFilled, Filled, NotFilled )
    , ( NotFilled, Filled, Filled, Filled )
    )


squashCellState rowIndex ( index, cell ) =
    ( rowIndex, index, cell )


isFilled ( rowIndex, columnIndex, cell ) =
    cell == Filled


coordinatize ( rowIndex, columnIndex, cell ) =
    ( rowIndex, columnIndex )


flattenRow ( rowIndex, row ) =
    row
        |> List.indexedMap (,)
        |> List.map (squashCellState rowIndex)
        |> List.filter isFilled
        |> List.map coordinatize


gridToCoordinateList pieceGrid =
    pieceGrid
        |> pieceGridTo2DList
        |> List.indexedMap (,)
        |> List.concatMap flattenRow


layoutToGrid : PieceLayout -> Grid
layoutToGrid layout =
    layout
        |> List.indexedMap (,)
        |> List.concatMap flattenRow


pieceRowToList ( cellState1, cellState2, cellState3, cellState4 ) =
    [ cellState1, cellState2, cellState3, cellState4 ]


pieceGridTo2DList ( pieceRow1, pieceRow2, pieceRow3, pieceRow4 ) =
    List.map pieceRowToList [ pieceRow1, pieceRow2, pieceRow3, pieceRow4 ]


renderPieceInstance : PieceInstance -> Html Msg
renderPieceInstance { pieceGrid, coordinate, color } =
    let
        pieceCoordinateList =
            gridToCoordinateList pieceGrid
    in
        div []
            (List.map
                (renderSquare coordinate color)
                pieceCoordinateList
            )


renderSquare coordinate color ( rowIndex, columnIndex ) =
    div [ class "square", style (squareStyle coordinate color rowIndex columnIndex) ] []


toPixels integer =
    let
        string =
            toString integer
    in
        string ++ "px"


pieceColorCode : Color -> String
pieceColorCode color =
    case color of
        Yellow ->
            "yellow"

        Blue ->
            "blue"

        Red ->
            "red"

        Green ->
            "green"

        Purple ->
            "purple"

        Cyan ->
            "cyan"

        Orange ->
            "orange"


squareStyle ( xCoordinate, yCoordinate ) color columnIndex rowIndex =
    [ ( "left", (rowIndex + xCoordinate) * 33 |> toPixels )
    , ( "top", (columnIndex + yCoordinate) * 33 |> toPixels )
    , ( "background-color", pieceColorCode color )
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.Extra.downs KeyDown
        , AnimationFrame.diffs TimeUpdate
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
