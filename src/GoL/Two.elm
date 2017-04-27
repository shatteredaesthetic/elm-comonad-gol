module GoL.Two exposing (..)

import List.Extra exposing (lift2)
import Data.ZipW as Z exposing (ZipW(..))
import Data.ZipW2d as Z2 exposing (ZipW2d)


aliveNeighbours : ZipW2d Bool -> Int
aliveNeighbours z =
    let
        horiz =
            [ Z.map Z.prev, Z.map Z.next ]

        vert =
            [ Z.prev, Z.next ]
    in
        [ horiz, vert, lift2 (<<) horiz vert ]
            |> List.concat
            |> List.map (\dir -> Z2.extract <| dir z)
            |> foldBool


foldBool : List Bool -> Int
foldBool =
    List.length << List.filter ((==) True)


rule : ZipW2d Bool -> Bool
rule z =
    case aliveNeighbours z of
        2 ->
            Z2.extract z

        3 ->
            True

        _ ->
            False


evolve : ZipW2d Bool -> ZipW2d Bool
evolve =
    Z2.extend rule


glider : ZipW2d Bool
glider =
    let
        rs =
            [ line [ False, True, False ]
            , line [ False, False, True ]
            , line [ True, True, True ]
            ]
                |> (flip List.append) (List.repeat 3 fz)

        fl =
            List.repeat 6 False

        fz =
            Zip (List.repeat 5 False) False fl

        line l =
            Zip (List.repeat 5 False) False (List.take 6 <| l ++ fl)
    in
        Zip (List.repeat 5 fz) fz rs
