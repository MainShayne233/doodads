module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    { text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { text = "..." }, Cmd.none )



---- UPDATE ----


type Msg
    = Change String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newText ->
            ( { model | text = newText }, Cmd.none )

        other ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Doodads" ]
        , renderPiece rightHookLayout
        ]


type CellState
    = Filled
    | NotFilled


type alias Row =
    List CellState


type alias Layout =
    List Row


type alias Coordinate =
    ( Int, Int )


type alias Grid =
    List Coordinate


fourSquareLayout : Layout
fourSquareLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, NotFilled, NotFilled ]
    ]


longOneLayout : Layout
longOneLayout =
    [ [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    ]


leftNoodleLayout : Layout
leftNoodleLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    ]


rightNoodleLayout : Layout
rightNoodleLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    ]


leftHookLayout : Layout
leftHookLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    , [ NotFilled, NotFilled, Filled, NotFilled ]
    ]


rightHookLayout : Layout
rightHookLayout =
    [ [ NotFilled, NotFilled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, Filled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    , [ NotFilled, Filled, NotFilled, NotFilled ]
    ]


triGuyLayout : Layout
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


layoutToGrid : Layout -> Grid
layoutToGrid layout =
    layout
        |> List.indexedMap (,)
        |> List.concatMap flattenRow


renderPiece : Layout -> Html Msg
renderPiece layout =
    layout
        |> layoutToGrid
        |> renderGrid


renderGrid : Grid -> Html Msg
renderGrid grid =
    div []
        (List.map
            renderSauare
            grid
        )


renderSauare ( rowIndex, columnIndex ) =
    div [ class "square", style (squareStyle rowIndex columnIndex) ] []


toPixels integer =
    let
        string =
            toString integer
    in
        string ++ "px"


squareStyle columnIndex rowIndex =
    [ ( "left", rowIndex * 33 |> toPixels )
    , ( "top", columnIndex * 33 |> toPixels )
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
