module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Time



---- MODEL ----


type Cycle
    = Work
    | ShortBreak
    | LongBreak


nextCycle : Cycle -> Int -> Cycle
nextCycle cycle breaks =
    case cycle of
        Work ->
            if breaks >= 4 then
                LongBreak

            else
                ShortBreak

        ShortBreak ->
            Work

        LongBreak ->
            Work


cycleToString : Cycle -> String
cycleToString cycle =
    case cycle of
        Work ->
            "Trabalho"

        ShortBreak ->
            "Descanso curto"

        LongBreak ->
            "Descanso longo"


cycleSize : Cycle -> Int
cycleSize cycle =
    case cycle of
        Work ->
            10

        ShortBreak ->
            5

        LongBreak ->
            9


type alias Model =
    { currentCycle : Cycle
    , currentTime : Int
    , breaks : Int
    , paused : Bool
    }


initialModel =
    { currentCycle = Work, currentTime = 0, paused = True, breaks = 0 }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Tick
    | Start
    | Pause
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            if model.currentTime >= cycleSize model.currentCycle then
                let
                    breaks =
                        if model.currentCycle == ShortBreak then
                            model.breaks + 1

                        else
                            model.breaks
                in
                ( { model
                    | currentTime = 0
                    , currentCycle = nextCycle model.currentCycle model.breaks
                    , breaks = breaks
                  }
                , Cmd.none
                )

            else
                ( { model | currentTime = model.currentTime + 1 }, Cmd.none )

        Start ->
            ( { model | paused = False }, Cmd.none )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Reset ->
            ( initialModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ text "Pomodoro App"
        , div [] [ text (cycleToString model.currentCycle) ]
        , div [] [ text (String.fromInt model.currentTime) ]
        , div []
            [ text
                (if model.paused == True then
                    "Parado"

                 else
                    "Rodando"
                )
            ]
        , button [ onClick Start ] [ text "Começar" ]
        , button [ onClick Pause ] [ text "Parar" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every 1000 (\_ -> Tick)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
