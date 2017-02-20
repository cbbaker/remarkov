module Collision exposing (..)

type alias Point = (Float, Float)
type alias Vector = (Float, Float)

type alias Boundary =
    { normal : Vector
    , offset : Float
    }

distanceToBoundary : Point -> Boundary -> Float
distanceToBoundary (x, y) {normal, offset} =
    let
        (nx, ny) = normal
    in
        x * nx + y * ny + offset

type alias Collision = 
    { distance : Float
    , location : Point
    , boundary : Boundary
    }

boundaries : Float -> Float -> List Boundary
boundaries width height =
    let 
        halfWidth = width / 2.0
        halfHeight = height / 2.0
    in
        [ Boundary ( 1.0,  0.0) halfWidth
        , Boundary (-1.0,  0.0) halfWidth
        , Boundary ( 0.0,  1.0) halfHeight
        , Boundary ( 0.0, -1.0) halfHeight
        ]

maybeMin : Maybe Collision -> Maybe Collision -> Maybe Collision
maybeMin maybeLeft maybeRight =
    case maybeLeft of
        Just left ->
            case maybeRight of
                Just right ->
                    if left.distance <= right.distance then
                        maybeLeft
                    else
                        maybeRight

                Nothing ->
                    maybeLeft

        Nothing ->
            maybeRight

collides : Point -> Point -> Boundary -> Maybe Collision
collides (x1, y1) (x2, y2) boundary =
    let
        (nx, ny) = boundary.normal
        d2 = distanceToBoundary (x2, y2) boundary
    in
        if d2 < 0 then
            let
                d1 = distanceToBoundary (x1, y1) boundary
                ratio = d1 / (d1 - d2)
                (midX, midY) = (x1 + ratio * (x2 - x1), y1 + ratio * (y2 - y1))
            in
                Just <| Collision (-d2) (midX, midY) boundary
        else
            Nothing

firstCollision : Point -> Point -> List Boundary -> Maybe Collision
firstCollision p1 p2 =
    List.map (collides p1 p2) >> List.foldl maybeMin Nothing
    
reflect : Point -> Collision -> Point
reflect (x, y) {distance, boundary} =
    let
        (nx, ny) = boundary.normal
    in
        (x + 2.0 * distance * nx, y + 2.0 * distance * ny)

update : List Boundary -> List Point -> List Point
update boundaries points =
    case points of
        p2 :: p1 :: rest ->
            case firstCollision p1 p2 boundaries of
                Just collision ->
                    let
                        reflected = reflect p2 collision
                        midp = collision.location
                        newPoints = reflected :: midp :: p1 :: rest
                    in
                        update boundaries newPoints
                Nothing ->
                    points
                        

        _ ->
            points
