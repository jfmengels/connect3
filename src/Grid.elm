module Grid exposing
    ( Direction
    , Grid
    , Position
    , addCoin
    , init
    , moveSide
    , view
    , winner
    )

import Element exposing (Element)
import Element.Border as Border
import Element.Events as Events
import List.Extra as List
import Player exposing (Player(..))



-- MODEL


type Grid
    = Grid
        { columns : ( Column, Column, Column )
        , sides : ( Side, Side, Side )
        }


type Column
    = Empty
    | One Player
    | Two ( Player, Player )
    | Three ( Player, Player, Player )


type Side
    = Left
    | Center
    | Right


init : Grid
init =
    Grid
        { columns = ( Empty, Empty, Empty )
        , sides = ( Center, Center, Center )
        }



-- UPDATE


type Position
    = First
    | Second
    | Third


type Direction
    = GoLeft
    | GoRight


addCoin : Position -> Grid -> Player -> Maybe Grid
addCoin chosenColumn (Grid grid) player =
    let
        ( column1, column2, column3 ) =
            grid.columns
    in
    case chosenColumn of
        First ->
            addCoinToColumn player column1
                |> Maybe.map (\newColumn -> Grid { grid | columns = ( newColumn, column2, column3 ) })

        Second ->
            addCoinToColumn player column2
                |> Maybe.map (\newColumn -> Grid { grid | columns = ( column1, newColumn, column3 ) })

        Third ->
            addCoinToColumn player column3
                |> Maybe.map (\newColumn -> Grid { grid | columns = ( column1, column2, newColumn ) })


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


moveSide : ( Position, Direction ) -> Grid -> Maybe Grid
moveSide ( position, direction ) (Grid { columns, sides }) =
    let
        ( s1, s2, s3 ) =
            sides
    in
    case position of
        First ->
            moveSideTo direction s1
                |> Maybe.map
                    (\newSide ->
                        Grid
                            { columns = columns
                            , sides = ( newSide, s2, s3 )
                            }
                    )

        Second ->
            moveSideTo direction s2
                |> Maybe.map
                    (\newSide ->
                        Grid
                            { columns = columns
                            , sides = ( s1, newSide, s3 )
                            }
                    )

        Third ->
            moveSideTo direction s3
                |> Maybe.map
                    (\newSide ->
                        Grid
                            { columns = columns
                            , sides = ( s1, s2, newSide )
                            }
                    )


moveSideTo : Direction -> Side -> Maybe Side
moveSideTo direction side =
    case direction of
        GoLeft ->
            case side of
                Left ->
                    Nothing

                Center ->
                    Just Left

                Right ->
                    Just Center

        GoRight ->
            case side of
                Left ->
                    Just Center

                Center ->
                    Just Right

                Right ->
                    Nothing



-- RESOLUTION


winner : Grid -> Maybe Player
winner (Grid { columns }) =
    let
        ( ( a, b, c ), ( d, e, f ), ( g, h, i ) ) =
            createGrid columns

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


createGrid : ( Column, Column, Column ) -> ( PlayerColumn, PlayerColumn, PlayerColumn )
createGrid ( c1, c2, c3 ) =
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


view : { onColumnClick : Position -> msg, onSideClick : ( Position, Direction ) -> msg } -> Grid -> Element msg
view { onColumnClick, onSideClick } (Grid grid) =
    let
        ( c1, c2, c3 ) =
            grid.columns

        columns : List ( Position, Column )
        columns =
            [ ( First, c1 ), ( Second, c2 ), ( Third, c3 ) ]
    in
    Element.row
        [ Element.padding 40 ]
        ([ viewOuterLeftSide onSideClick grid.sides
         , viewInnerLeftSide onSideClick grid.sides
         ]
            ++ List.map (viewColumn onColumnClick) columns
            ++ [ viewInnerRightSide onSideClick grid.sides
               , viewOuterRightSide onSideClick grid.sides
               ]
        )


viewColumn : (Position -> msg) -> ( Position, Column ) -> Element msg
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


viewOuterLeftSide : (( Position, Direction ) -> msg) -> ( Side, Side, Side ) -> Element msg
viewOuterLeftSide onSideClick sides =
    sides
        |> sidesWithPositions
        |> List.map
            (\( side, position ) ->
                if side == Left then
                    viewLeftSide onSideClick position

                else
                    viewEmptySide
            )
        |> Element.column []


viewInnerLeftSide : (( Position, Direction ) -> msg) -> ( Side, Side, Side ) -> Element msg
viewInnerLeftSide onSideClick sides =
    sides
        |> sidesWithPositions
        |> List.map
            (\( side, position ) ->
                if side /= Right then
                    viewLeftSide onSideClick position

                else
                    viewEmptySide
            )
        |> Element.column []


viewOuterRightSide : (( Position, Direction ) -> msg) -> ( Side, Side, Side ) -> Element msg
viewOuterRightSide onSideClick sides =
    sides
        |> sidesWithPositions
        |> List.map
            (\( side, position ) ->
                if side == Right then
                    viewRightSide onSideClick position

                else
                    viewEmptySide
            )
        |> Element.column []


viewInnerRightSide : (( Position, Direction ) -> msg) -> ( Side, Side, Side ) -> Element msg
viewInnerRightSide onSideClick sides =
    sides
        |> sidesWithPositions
        |> List.map
            (\( side, position ) ->
                if side /= Left then
                    viewRightSide onSideClick position

                else
                    viewEmptySide
            )
        |> Element.column []


sidesWithPositions : ( Side, Side, Side ) -> List ( Side, Position )
sidesWithPositions ( side1, side2, side3 ) =
    [ ( side1, First )
    , ( side2, Second )
    , ( side3, Third )
    ]


viewLeftSide : (( Position, Direction ) -> msg) -> Position -> Element msg
viewLeftSide onSideClick position =
    Element.el [ Events.onClick (onSideClick ( position, GoRight )) ] <|
        Element.text ">"


viewRightSide : (( Position, Direction ) -> msg) -> Position -> Element msg
viewRightSide onSideClick position =
    Element.el [ Events.onClick (onSideClick ( position, GoLeft )) ] <|
        Element.text "<"


viewEmptySide : Element msg
viewEmptySide =
    Element.text " "
