module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)
import Random
import Random.Float
import Time exposing (..)
import Collision
import Task
import Window


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { drawLength : Distribution
    , updateInterval : Maybe Time
    , drawing : Drawing
    , size : Window.Size
    , timing : Timing
    }


type alias Distribution =
    { mean : Float
    , stddev : Float
    }


type alias Drawing =
    { points : List ( Float, Float )
    }


type alias Timing =
    { last : Float
    , delta : Float
    }


type Msg
    = Tick Time
    | NewLine ( Float, Float )
    | Resize Window.Size


init : ( Model, Cmd Msg )
init =
    Model (Distribution 0.0 20.0)
        (Just 33.3)
        (Drawing [ ( 0.0, 0.0 ) ])
        (Window.Size 640 480)
        (Timing 0.0 300.0)
        ! [ Task.perform Resize Window.size ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            case model.updateInterval of
                Just time ->
                    [ every time Tick ]

                Nothing ->
                    []

        withSize =
            (Window.resizes Resize) :: subs
    in
        Sub.batch withSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                delta =
                    time - model.timing.last

                newDelta =
                    0.05 * delta + 0.95 * model.timing.delta

                timing =
                    Timing time newDelta
            in
                { model | timing = timing }
                    ! [ Random.generate NewLine <| newLineGenerator model.drawLength ]

        NewLine ( dx, dy ) ->
            { model | drawing = updateDrawing ( dx, dy ) model } ! []

        Resize size ->
            { model | size = size } ! []


updateDrawing : ( Float, Float ) -> Model -> Drawing
updateDrawing ( dx, dy ) { drawing, size } =
    case drawing.points of
        ( x, y ) :: tail ->
            let
                boundaries =
                    Collision.boundaries (toFloat size.width) (toFloat size.height)

                newPoints =
                    ( x + dx, y + dy ) :: ( x, y ) :: tail
            in
                { drawing | points = Collision.update boundaries newPoints }

        [] ->
            drawing


newLineGenerator : Distribution -> Random.Generator ( Float, Float )
newLineGenerator { stddev, mean } =
    let
        coord =
            Random.Float.normal mean stddev
    in
        Random.pair coord coord


view : Model -> Html Msg
view model =
    div [ HtmlAttr.style [ ( "position", "relative" ) ] ]
        [ viewSvg model
        , fpsCounter model
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    div [ HtmlAttr.style [ ( "position", "relative" ) ] ]
        [ svg
            [ SvgAttr.width <| toString model.size.width
            , SvgAttr.height <| toString model.size.height
            , viewBox <| bounds model.size
            ]
          <|
            viewLines model
        ]


bounds : Window.Size -> String
bounds { width, height } =
    let
        left =
            -width // 2

        top =
            -height // 2
    in
        [ left, top, width, height ]
            |> List.map toString
            |> String.join " "


fpsCounter : Model -> Html Msg
fpsCounter { timing, size } =
    let
        fps =
            toFloat (round (100000.0 / timing.delta)) / 100.0
    in
        div
            [ HtmlAttr.style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "bottom", "10px" )
                , ( "font-size", "72px" )
                , ( "background", "rgba(1,1,1,0.5)" )
                , ( "color", "lightgray" )
                , ( "padding", "10px" )
                ]
            ]
            [ Html.text (toString fps) ]


viewLines : Model -> List (Html Msg)
viewLines model =
    let
        mkLine ( x2, y2 ) ( ( x1, y1 ), output ) =
            let
                elem =
                    line
                        [ SvgAttr.x1 (toString x1)
                        , SvgAttr.y1 (toString y1)
                        , SvgAttr.x2 (toString x2)
                        , SvgAttr.y2 (toString y2)
                        , SvgAttr.fill "none"
                        , SvgAttr.stroke "black"
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.strokeLinecap "round"
                        ]
                        []
            in
                ( ( x2, y2 ), elem :: output )
    in
        case model.drawing.points of
            head :: tail ->
                Tuple.second <| List.foldl mkLine ( head, [] ) tail

            [] ->
                []
