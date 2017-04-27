module Main exposing (..)

import App exposing (..)
import Html exposing (program, Html, text, div, button)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Data.ZipW as Z exposing (ZipW)
import GoL.One as G1


init : ( Model, Cmd Msg )
init =
    { pattern = One { seq = G1.line, evolve = G1.evolve }
    , playing = False
    }
        ! []


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ div
            [ class "ctrl-panel" ]
            [ btnRow model.playing
            ]
        , div
            [ class "gol-container" ]
            [ golView model.pattern ]
        ]


btnRow : Bool -> Html Msg
btnRow bool =
    div
        [ class "btn-container" ]
        [ button [ onClick Next, class "btn" ] [ text "Next Frame" ]
        , button
            [ onClick Toggle, class "btn" ]
            [ text <|
                if bool then
                    "Stop"
                else
                    "Play"
            ]
        , button [ onClick Reset, class "btn" ] [ text "Reset" ]
        ]


cellView : Bool -> Html Msg
cellView b =
    div
        [ classList
            [ ( "cell", True )
            , ( "alive", b )
            ]
        ]
        []


golView : Pattern Bool -> Html Msg
golView (One p) =
    div [ class "container-1d" ] <| Z.toList <| Z.map cellView p.seq


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
