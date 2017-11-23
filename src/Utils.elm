module Utils exposing (..)

zipWithIndex : List a -> List (Int, a)
zipWithIndex ls =
    let
        apply xs idxs =
            case (xs, idxs) of
                (x :: xs, idx :: idxs) -> (idx, x) :: (apply xs idxs)
                ([], []) -> []
                _ -> []
    in
        apply ls (List.range 1 (List.length ls))

takeNotWhile : (a -> Bool) -> List a -> List a
takeNotWhile f ls =
    case ls of
        x :: xs -> if f x then [] else x :: (takeNotWhile f xs)
        [] -> []

dropNotWhile : (a -> Bool) -> List a -> List a
dropNotWhile f ls =
    case ls of
        x :: xs -> if not (f x) then dropNotWhile f xs else xs
        [] -> []

updateOnWay : List a -> a -> (a -> a) -> List a
updateOnWay ls elem f =
    let
        -- 特定の要素を境目に、リストを分割する
        -- [a, b, c, d, e] -> [a, b], [d, e]
        leftSide = takeNotWhile (\x -> x == elem) ls
        rightSide = dropNotWhile (\x -> x == elem) ls
        updated = f elem
    in
        leftSide ++ [updated] ++ rightSide