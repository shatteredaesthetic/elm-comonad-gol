module Main exposing (..)

import App exposing (..)
import Html exposing (program, Html, text, div, button)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Data.ZipW as Z exposing (ZipW)
import Data.ZipW2d as Z2 exposing (ZipW2d)
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
            , dimensionView model.pattern
            ]
        , div
            [ class "gol-container" ]
            [ golView model.pattern ]
        ]


dimensionView : Pattern Bool -> Html Msg
dimensionView pattern =
    case pattern of
        One _ ->
            div
                [ class "dimension"
                , onClick Dimension
                ]
                [ text "...to the 2nd Dimension" ]

        Two _ ->
            div
                [ classList [ ( "dimension", True ), ( "select-2d", True ) ]
                , onClick Dimension
                ]
                [ text "...to the 1st Dimension" ]


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
golView pattern =
    case pattern of
        One p ->
            div [ class "container-1d" ] <| Z.toList <| Z.map cellView p.seq

        Two p ->
            div [ class "container-2d" ] <| Z2.toList <| Z2.map cellView p.seq


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
