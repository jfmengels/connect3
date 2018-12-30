module Main exposing (main)

import Browser
import Element
import Grid exposing (Grid)
import Html exposing (Html)
import Player exposing (Player)



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
    = AddCoin Grid.ChosenColumn


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



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Grid.view AddCoin model.grid
