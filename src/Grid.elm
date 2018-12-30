module Grid exposing (ChosenColumn, Grid, addCoin, init, view)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events as Events
import Player exposing (Player(..))



-- MODEL


type Grid
    = Grid ( Column, Column, Column )


type Column
    = Empty
    | One Player
    | Two ( Player, Player )
    | Three ( Player, Player, Player )


init : Grid
init =
    Grid ( Empty, Empty, Empty )



-- UPDATE


type ChosenColumn
    = First
    | Second
    | Third


addCoin : ChosenColumn -> Grid -> Player -> Maybe Grid
addCoin chosenColumn (Grid ( column1, column2, column3 )) player =
    case chosenColumn of
        First ->
            addCoinToColumn player column1
                |> Maybe.map (\newColumn -> Grid ( newColumn, column2, column3 ))

        Second ->
            addCoinToColumn player column2
                |> Maybe.map (\newColumn -> Grid ( column1, newColumn, column3 ))

        Third ->
            addCoinToColumn player column3
                |> Maybe.map (\newColumn -> Grid ( column1, column2, newColumn ))


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


view : (ChosenColumn -> msg) -> Grid -> Html msg
view onClick (Grid ( c1, c2, c3 )) =
    let
        columns : List ( ChosenColumn, Column )
        columns =
            [ ( First, c1 ), ( Second, c2 ), ( Third, c3 ) ]
    in
    div [] (List.map (viewColumn onClick) columns)


viewColumn : (ChosenColumn -> msg) -> ( ChosenColumn, Column ) -> Html msg
viewColumn onClick ( chosenColumn, column ) =
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

        children : List (Html msg)
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
            Events.onClick (onClick chosenColumn)

          else
            Attr.class ""
        ]
        children


viewEmpty : Html msg
viewEmpty =
    text "-"


viewCoin : Player -> Html msg
viewCoin player =
    case player of
        X ->
            text "X"

        O ->
            text "O"
