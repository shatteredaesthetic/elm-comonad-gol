module Data.ZipW2d exposing (..)

import Data.ZipW as Z exposing (ZipW(..))


type alias ZipW2d a =
    ZipW (ZipW a)


singleton : a -> ZipW2d a
singleton =
    Z.duplicate << Z.singleton


toList : ZipW2d a -> List a
toList =
    List.concat << Z.toList << Z.map Z.toList


map : (a -> b) -> ZipW2d a -> ZipW2d b
map f =
    Z.map <| Z.map f


extract : ZipW2d a -> a
extract =
    Z.extract << Z.extract


duplicate : ZipW2d a -> ZipW2d (ZipW2d a)
duplicate =
    Z.map horizontal << vertical


extend : (ZipW2d a -> b) -> ZipW2d a -> ZipW2d b
extend f =
    duplicate >> map f


write : a -> ZipW2d a -> ZipW2d a
write x z =
    Z.extract z
        |> Z.write x
        |> (flip Z.write) z


halfList : List a -> ( Bool, Int )
halfList list =
    ( rem (List.length list) 2 == 0, List.length list // 2 )


iter : (a -> a) -> Int -> a -> List a
iter f n x =
    Z.repeat f <| List.repeat n x


vertical : ZipW2d a -> ZipW (ZipW2d a)
vertical z =
    let
        ( isEven, len ) =
            halfList <| Z.toList z
    in
        if isEven then
            Zip (iter Z.prev (len - 1) z) z (iter Z.next len z)
        else
            Zip (iter Z.prev len z) z (iter Z.next len z)


horizontal : ZipW2d a -> ZipW (ZipW2d a)
horizontal z =
    let
        ( isEven, len ) =
            halfList <| Z.toList z
    in
        if isEven then
            Zip (iter (Z.map Z.prev) (len - 1) z) z (iter (Z.map Z.next) len z)
        else
            Zip (iter (Z.map Z.prev) len z) z (iter (Z.map Z.next) len z)
