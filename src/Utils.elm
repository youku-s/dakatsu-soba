module Utils exposing (..)

import Random.Pcg exposing (Seed, step)
import Uuid.Barebones exposing (uuidStringGenerator)
import List.Extra

zipWithUuid : Seed -> List a -> (Seed, List (String, a))
zipWithUuid seed list =
    let
        apply sd ls =
            case ls of
                x :: xs ->
                    let
                        (uuid, newSeed) = step uuidStringGenerator sd
                    in
                        (uuid, x, newSeed) :: apply newSeed xs
                [] -> []

        zipped = apply seed list

        lastSeed = 
            case List.Extra.last zipped of
                Just (_, _, sd) -> sd
                _ -> seed
    in
        (lastSeed, List.map (\(uuid, elem, seed) -> (uuid, elem)) zipped)
        
-- i番目の要素をj番目の位置に移動する
move : Int -> Int -> List a -> Maybe (List a)
move src dst ls =
    let
        (left, right) = List.partition (\(i, x) -> i < dst) (zipWithIndex ls)

        -- 移動する要素を探す
        target = List.head (List.drop src ls)

        -- 移動先にダミー要素を挿入しておく
        dummyInserted = Maybe.map (\x -> left ++ [(-1, x)] ++ right) target

        -- 移動済みの要素を削除する
        srcRemoved =
            Maybe.map
                (\xs -> 
                    List.filter 
                        (\(i, x) -> 
                            case target of
                                Just t -> i == -1 || t /= x
                                Nothing -> False
                        )
                        xs
                )
                dummyInserted
    in
        Maybe.map (\ls -> List.map (\(i, x) -> x) ls) srcRemoved

insert : Int -> a -> List a -> List a
insert dst elem ls =
    let
        (left, right) = List.partition (\(i, x) -> i < dst) (zipWithIndex ls)

        inserted = right ++ [(-1, elem)] ++ left
    in
        List.map (\(i, x) -> x) inserted        

at : Int -> List a -> Maybe a
at index ls =
    List.head (List.drop index ls)

zipWithIndex : List a -> List (Int, a)
zipWithIndex ls =
    let
        apply xs idxs =
            case (xs, idxs) of
                (x :: xs, idx :: idxs) -> (idx, x) :: (apply xs idxs)
                ([], []) -> []
                _ -> []
    in
        apply ls (List.range 0 ((List.length ls) - 1))

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

updateOnWayUseEq : List a -> (a -> Bool) -> a -> (a -> a) -> List a
updateOnWayUseEq ls eq elem f =
    let
        leftSide = takeNotWhile eq ls
        rightSide = dropNotWhile eq ls
        updated = f elem
    in
        leftSide ++ [updated] ++ rightSide