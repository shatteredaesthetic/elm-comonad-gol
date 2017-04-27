module Data.ZipW exposing (..)


type ZipW a
    = Zip (List a) a (List a)


singleton : a -> ZipW a
singleton x =
    Zip [] x []


fromList : a -> List a -> ZipW a
fromList default list =
    case list of
        [] ->
            Zip [] default []

        x :: xs ->
            let
                half =
                    List.length list // 2
            in
                Zip (List.drop half xs) x (List.take half xs)


toList : ZipW a -> List a
toList (Zip l c r) =
    List.reverse l ++ [ c ] ++ r


next : ZipW a -> ZipW a
next ((Zip l c r) as z) =
    case r of
        [] ->
            z

        r :: rs ->
            let
                ( first, ls ) =
                    Maybe.withDefault ( c, [] ) <| unconsBack l
            in
                Zip (c :: ls) r (rs ++ [ first ])


prev : ZipW a -> ZipW a
prev ((Zip l c r) as z) =
    case l of
        [] ->
            z

        l :: ls ->
            let
                ( last, rs ) =
                    Maybe.withDefault ( c, [] ) <| unconsBack r
            in
                Zip (ls ++ [ last ]) l (c :: rs)


map : (a -> b) -> ZipW a -> ZipW b
map f (Zip l c r) =
    Zip (List.map f l) (f c) (List.map f r)


extract : ZipW a -> a
extract (Zip _ c _) =
    c


duplicate : ZipW a -> ZipW (ZipW a)
duplicate z =
    let
        len =
            (List.length <| toList z) // 2

        iterate f n x =
            repeat f <| List.repeat n x
    in
        Zip (iterate prev (len - 1) z) z (iterate next len z)


extend : (ZipW a -> b) -> ZipW a -> ZipW b
extend f =
    duplicate >> map f


write : a -> ZipW a -> ZipW a
write c (Zip l _ r) =
    Zip l c r



-- Utilities


repeat : (a -> a) -> List a -> List a
repeat f l =
    case List.map f l of
        [] ->
            []

        x :: xs ->
            x :: repeat f xs


unconsBack : List a -> Maybe ( a, List a )
unconsBack l =
    case List.reverse l of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, List.reverse xs )
