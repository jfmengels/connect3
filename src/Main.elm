module Main exposing (main)

import Browser
import Element
import Grid exposing (Grid)
import Html exposing (Html)
import Player exposing (Player(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { currentPlayer : Player
    , grid : Grid
    }


init : Model
init =
    { currentPlayer = Player.X
    , grid = Grid.init
    }



-- UPDATE


type Msg
    = AddCoin Grid.Position
    | MoveSide ( Grid.Position, Grid.Direction )


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCoin chosenColumn ->
            case Grid.addCoin chosenColumn model.grid model.currentPlayer of
                Nothing ->
                    model

                Just grid ->
                    { model
                        | currentPlayer = Player.switch model.currentPlayer
                        , grid = grid
                    }

        MoveSide data ->
            case Grid.moveSide data model.grid of
                Nothing ->
                    model

                Just grid ->
                    { model
                        | currentPlayer = Player.switch model.currentPlayer
                        , grid = grid
                    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column []
            [ Grid.view { onColumnClick = AddCoin, onSideClick = MoveSide } model.grid
            , case Grid.winner model.grid of
                Just Player.X ->
                    Element.text "X has won"

                Just Player.O ->
                    Element.text "O has won"

                Nothing ->
                    Element.none
            ]
