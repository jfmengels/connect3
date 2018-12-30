module Grid exposing (ChosenColumn, Grid, addCoin, init, view, winner)

import Element exposing (Element)
import Element.Border as Border
import Element.Events as Events
import List.Extra as List
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



-- RESOLUTION


winner : Grid -> Maybe Player
winner grid_ =
    let
        ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) =
            createGrid grid_

        combinations : List ( Maybe Player, Maybe Player, Maybe Player )
        combinations =
            [ ( a, b, c )
            , ( d, e, f )
            , ( g, h, i )
            , ( a, d, g )
            , ( b, e, h )
            , ( c, f, i )
            , ( a, e, i )
            , ( c, e, g )
            ]
    in
    combinations
        |> List.find (\( p1, p2, p3 ) -> p1 /= Nothing && p1 == p2 && p2 == p3)
        |> Maybe.andThen (\( p1, p2, p3 ) -> p1)


type alias PlayerColumn =
    ( Maybe Player, Maybe Player, Maybe Player )


createGrid : Grid -> ( PlayerColumn, PlayerColumn, PlayerColumn )
createGrid (Grid ( c1, c2, c3 )) =
    ( createRow c1, createRow c2, createRow c3 )


createRow : Column -> PlayerColumn
createRow column =
    case column of
        Empty ->
            ( Nothing, Nothing, Nothing )

        One player ->
            ( Just player, Nothing, Nothing )

        Two ( player, player2 ) ->
            ( Just player, Just player2, Nothing )

        Three ( player, player2, player3 ) ->
            ( Just player, Just player2, Just player3 )



-- VIEW


view : (ChosenColumn -> msg) -> Grid -> Element msg
view onClick (Grid ( c1, c2, c3 )) =
    let
        columns : List ( ChosenColumn, Column )
        columns =
            [ ( First, c1 ), ( Second, c2 ), ( Third, c3 ) ]
    in
    Element.row
        [ Element.padding 40 ]
        (List.map (viewColumn onClick) columns)


viewColumn : (ChosenColumn -> msg) -> ( ChosenColumn, Column ) -> Element msg
viewColumn onClick ( chosenColumn, column ) =
    let
        isFilled : Bool
        isFilled =
            case column of
                Empty ->
                    False

                One _ ->
                    False

                Two _ ->
                    False

                Three _ ->
                    True

        onClickAttributes : List (Element.Attribute msg)
        onClickAttributes =
            if not isFilled then
                [ Events.onClick <| onClick chosenColumn
                , Element.mouseOver
                    [ Border.color <| Element.rgb 1 0 0
                    ]
                ]

            else
                []

        children : List (Element msg)
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
    Element.column
        ([ Element.width (Element.px 20)
         , Element.spacingXY -1 5
         , Element.pointer
         , Border.solid
         , Border.width 1
         ]
            ++ onClickAttributes
        )
        (List.reverse children)


viewEmpty : Element msg
viewEmpty =
    Element.el [ Element.centerX ] <|
        Element.text "-"


viewCoin : Player -> Element msg
viewCoin player =
    Element.el [ Element.centerX ] <|
        case player of
            X ->
                Element.text "X"

            O ->
                Element.text "O"
