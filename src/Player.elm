module Player exposing (Player(..), switch)

-- MODEL


type Player
    = X
    | O



-- UPDATE


switch : Player -> Player
switch player =
    case player of
        X ->
            O

        O ->
            X
