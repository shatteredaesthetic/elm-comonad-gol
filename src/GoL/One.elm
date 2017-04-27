module GoL.One exposing (..)

import Data.ZipW as Z exposing (ZipW)


rule : ZipW Bool -> Bool
rule z =
    ( Z.prev, Z.next )
        |> tupAp z
        |> tupMap Z.extract
        |> ruleTest


ruleTest : ( Bool, Bool ) -> Bool
ruleTest ( p, n ) =
    p && not n || n && not p


evolve : ZipW Bool -> ZipW Bool
evolve =
    Z.extend rule


tupAp : a -> ( a -> b, a -> b ) -> ( b, b )
tupAp x ( f, g ) =
    ( f x, g x )


tupMap : (a -> b) -> ( a, a ) -> ( b, b )
tupMap f ( x, y ) =
    ( f x, f y )


line : ZipW Bool
line =
    Z.fromList False <| [ False, True, False, True, True ] ++ List.repeat 5 False ++ [ True, True, False, True ]
