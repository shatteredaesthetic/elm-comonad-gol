module App exposing (..)

import Time exposing (Time, millisecond)
import Task exposing (..)
import Process exposing (sleep)
import Data.ZipW as Z exposing (ZipW)
import GoL.One as G1 exposing (line)


type Pattern a
    = One
        { seq : ZipW a
        , evolve : ZipW a -> ZipW a
        }


type alias Model =
    { pattern : Pattern Bool
    , playing : Bool
    }


type Msg
    = Next
    | Tick
    | Toggle
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            step model ! []

        Tick ->
            if model.playing then
                step model ! [ delay (500 * millisecond) Tick ]
            else
                model ! []

        Toggle ->
            { model | playing = not model.playing } ! [ delay (500 * millisecond) Tick ]

        Reset ->
            { model | pattern = One { seq = line, evolve = G1.evolve }, playing = False } ! []


step : Model -> Model
step model =
    let
        (One patt) =
            model.pattern

        patt_ =
            One { patt | seq = patt.evolve patt.seq }
    in
        { model | pattern = patt_ }


delay : Time -> Msg -> Cmd Msg
delay time msg =
    sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
