module App exposing (..)

import Time exposing (Time, millisecond)
import Task exposing (..)
import Process exposing (sleep)
import Data.ZipW as Z exposing (ZipW)
import Data.ZipW2d as Z2 exposing (ZipW2d)
import GoL.One as G1 exposing (line)
import GoL.Two as G2 exposing (glider)


type Pattern a
    = One
        { seq : ZipW a
        , evolve : ZipW a -> ZipW a
        }
    | Two
        { seq : ZipW2d a
        , evolve : ZipW2d a -> ZipW2d a
        }


type alias Model =
    { pattern : Pattern Bool
    , playing : Bool
    }


type Msg
    = Next
    | Tick
    | Toggle
    | Dimension
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

        Dimension ->
            case model.pattern of
                One _ ->
                    { model | pattern = Two { seq = glider, evolve = G2.evolve } } ! []

                Two _ ->
                    { model | pattern = One { seq = line, evolve = G1.evolve } } ! []

        Reset ->
            { model | pattern = One { seq = line, evolve = G1.evolve }, playing = False } ! []


step : Model -> Model
step model =
    let
        nxt m =
            { m | seq = m.evolve m.seq }
    in
        case model.pattern of
            One p ->
                { model | pattern = One <| nxt p }

            Two p ->
                { model | pattern = Two <| nxt p }


delay : Time -> Msg -> Cmd Msg
delay time msg =
    sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
