module Main exposing (main)

import Browser
import Html exposing (Html, div, text)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Model
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    {}



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Hi" ]
