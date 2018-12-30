module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events as Events



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
    , columns : ( Column, Column, Column )
    }


type Column
    = Empty
    | One Player
    | Two ( Player, Player )
    | Three ( Player, Player, Player )


type Player
    = X
    | O


init : Model
init =
    { currentPlayer = X
    , columns = ( Empty, Empty, Empty )
    }



-- UPDATE


type Msg
    = AddCoin ChosenColumn


type ChosenColumn
    = First
    | Second
    | Third


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCoin chosenColumn ->
            case addCoin chosenColumn model.columns model.currentPlayer of
                Nothing ->
                    model

                Just columns ->
                    { model
                        | currentPlayer = switchPlayer model.currentPlayer
                        , columns = columns
                    }


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        X ->
            O

        O ->
            X


addCoin : ChosenColumn -> ( Column, Column, Column ) -> Player -> Maybe ( Column, Column, Column )
addCoin chosenColumn ( column1, column2, column3 ) player =
    case chosenColumn of
        First ->
            addCoinToColumn player column1
                |> Maybe.map (\newColumn -> ( newColumn, column2, column3 ))

        Second ->
            addCoinToColumn player column2
                |> Maybe.map (\newColumn -> ( column1, newColumn, column3 ))

        Third ->
            addCoinToColumn player column3
                |> Maybe.map (\newColumn -> ( column1, column2, newColumn ))


addCoinToColumn : Player -> Column -> Maybe Column
addCoinToColumn player column =
    case column of
        Empty ->
            Just <| One player

        One a ->
            Just <| Two ( a, player )

        Two ( a, b ) ->
            Just <| Three ( a, b, player )

        Three _ ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( c1, c2, c3 ) =
            model.columns

        columns : List ( ChosenColumn, Column )
        columns =
            [ ( First, c1 ), ( Second, c2 ), ( Third, c3 ) ]
    in
    div [] (List.map viewColumn columns)


viewColumn : ( ChosenColumn, Column ) -> Html Msg
viewColumn ( chosenColumn, column ) =
    let
        hasOnClick : Bool
        hasOnClick =
            case column of
                Empty ->
                    True

                One _ ->
                    True

                Two _ ->
                    True

                Three _ ->
                    False

        children : List (Html Msg)
        children =
            case column of
                Empty ->
                    [ viewEmpty, viewEmpty, viewEmpty ]

                One player ->
                    [ viewCoin player, viewEmpty, viewEmpty ]

                Two ( player, player2 ) ->
                    [ viewCoin player, viewCoin player2, viewEmpty ]

                Three ( player, player2, player3 ) ->
                    [ viewCoin player, viewCoin player2, viewCoin player3 ]
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , if hasOnClick then
            Events.onClick (AddCoin chosenColumn)

          else
            Attr.class ""
        ]
        children


viewEmpty : Html Msg
viewEmpty =
    text "-"


viewCoin : Player -> Html Msg
viewCoin player =
    case player of
        X ->
            text "X"

        O ->
            text "O"
