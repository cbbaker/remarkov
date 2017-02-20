module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random
import Random.Float
import Time exposing (..)
import Collision


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
    , boundaries : List Collision.Boundary
    }


type alias Distribution =
    { mean : Float
    , stddev : Float
    }


type alias Drawing =
    { points : List ( Float, Float )
    }


type alias Box =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }


type Msg
    = Tick Time
    | NewLine ( Float, Float )


init : ( Model, Cmd Msg )
init =
    Model (Distribution 0.0 20.0)
        (Just millisecond)
        (Drawing [ ( 0.0, 0.0 ) ])
        (Collision.boundaries 640.0 480.0)
        ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.updateInterval of
        Just time ->
            every time Tick

        Nothing ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            model ! [ Random.generate NewLine <| newLineGenerator model.drawLength ]

        NewLine (dx, dy) ->
            { model | drawing = updateDrawing (dx, dy) model } ! []


updateDrawing : (Float, Float) -> Model -> Drawing
updateDrawing (dx, dy) { drawing, boundaries }  =
    case drawing.points of
        (x, y) :: tail ->
            let
                newPoints = (x + dx, y + dy) :: (x, y) :: tail
            in
                { drawing | points = Collision.update boundaries newPoints  }

        [] ->
            drawing


updateBBox : ( Float, Float ) -> Box -> Box
updateBBox ( x, y ) bbox =
    let
        left =
            if x < bbox.left then
                x
            else
                bbox.left

        top =
            if y < bbox.top then
                y
            else
                bbox.top

        right =
            if x > bbox.right then
                x
            else
                bbox.right

        bottom =
            if y > bbox.bottom then
                y
            else
                bbox.bottom
    in
        Box left top right bottom


newLineGenerator : Distribution -> Random.Generator ( Float, Float )
newLineGenerator { stddev, mean } =
    let
        coord =
            Random.Float.normal mean stddev
    in
        Random.pair coord coord


view : Model -> Html Msg
view { drawing } =
    svg
        [ width "640"
        , height "480"
        , viewBox "-320 -240 640 480"
        ] <| viewLines drawing

bounds : Box -> String
bounds { left, top, right, bottom } =
    let
        width =
            right - left

        height =
            bottom - top
    in
        [ left, top, width, height ]
            |> List.map toString
            |> String.join " "


viewLines : Drawing -> List (Html Msg)
viewLines { points } =
    let
        mkLine ( x2, y2 ) ( ( x1, y1 ), output ) =
            let
                elem =
                    line
                        [ Svg.Attributes.x1 (toString x1)
                        , Svg.Attributes.y1 (toString y1)
                        , Svg.Attributes.x2 (toString x2)
                        , Svg.Attributes.y2 (toString y2)
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.stroke "black"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
            in
                ( ( x2, y2 ), elem :: output )
    in
        case points of
            head :: tail ->
                Tuple.second <| List.foldl mkLine ( head, [] ) tail

            [] ->
                []
