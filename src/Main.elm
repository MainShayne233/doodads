module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    { pieceInstances : List PieceInstance
    }


initPieceInstances : List PieceInstance
initPieceInstances =
    [ { piece = FourSquare, coordinate = ( 5, 5 ) } ]


init : ( Model, Cmd Msg )
init =
    ( { pieceInstances = initPieceInstances }, Cmd.none )



---- UPDATE ----


type Msg
    = Change String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        other ->
            ( model, Cmd.none )



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


renderPieceInstance : PieceInstance -> Html Msg
renderPieceInstance { piece, coordinate } =
    piece
        |> pieceSpec
        |> renderPiece coordinate


pieceSpec : Piece -> PieceSpec
pieceSpec piece =
    case piece of
        FourSquare ->
            fourSquarePieceSpec


type Piece
    = FourSquare



--    | LongOne
--    | LeftNoodle
--    | RightNoodle
--    | LeftHook
--    | RightHook
--    | TriGuy


type alias PieceInstance =
    { piece : Piece, coordinate : Coordinate }


type PieceColor
    = Yellow
    | Blue


type alias PieceSpec =
    { layout : PieceLayout
    , color : PieceColor
    }


type CellState
    = Filled
    | NotFilled


type alias Row =
    List CellState


type alias PieceLayout =
    List Row


type alias Coordinate =
    ( Int, Int )


type alias Grid =
    List Coordinate


fourSquareLayout : PieceLayout
fourSquareLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, NotFilled, NotFilled ]
    ]


fourSquarePieceSpec : PieceSpec
fourSquarePieceSpec =
    { color = Yellow
    , layout = fourSquareLayout
    }


longOneLayout : PieceLayout
longOneLayout =
    [ [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    ]


leftNoodleLayout : PieceLayout
leftNoodleLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    ]


rightNoodleLayout : PieceLayout
rightNoodleLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    ]


leftHookLayout : PieceLayout
leftHookLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    ]


rightHookLayout : PieceLayout
rightHookLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    ]


triGuyLayout : PieceLayout
triGuyLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    , [ NotFilled, Filled, Filled, Filled ]
    ]


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


layoutToGrid : PieceLayout -> Grid
layoutToGrid layout =
    layout
        |> List.indexedMap (,)
        |> List.concatMap flattenRow


renderPiece : Coordinate -> PieceSpec -> Html Msg
renderPiece coordinate { layout, color } =
    layout
        |> layoutToGrid
        |> renderGrid coordinate color


renderGrid : Coordinate -> PieceColor -> Grid -> Html Msg
renderGrid coordinate color grid =
    div []
        (List.map
            (renderSquare coordinate color)
            grid
        )


renderSquare coordinate color ( rowIndex, columnIndex ) =
    div [ class "square", style (squareStyle coordinate color rowIndex columnIndex) ] []


toPixels integer =
    let
        string =
            toString integer
    in
        string ++ "px"


pieceColorCode : PieceColor -> String
pieceColorCode color =
    case color of
        Yellow ->
            "yellow"

        other_ ->
            "white"


squareStyle ( xCoordinate, yCoordinate ) color columnIndex rowIndex =
    [ ( "left", (rowIndex + xCoordinate) * 33 |> toPixels )
    , ( "top", (columnIndex + yCoordinate) * 33 |> toPixels )
    , ( "background-color", pieceColorCode color )
    ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
