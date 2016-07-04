module Hexagon exposing (..)

import List
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Hexagon =
    { center : Point
    , rotation : Float
    , radius : Float
    , fill : String
    }


p : Float -> Float -> Point
p =
    Point


hex : Point -> Float -> Float -> String -> Hexagon
hex =
    Hexagon


main : Svg msg
main =
    svg [ viewBox "0 0 473.9 473.9", height "473.9", width "473.9" ]
        [ defs []
            [ linearGradient [ id "gradient", x1 "236.63", y1 "361.95", x2 "236.63", y2 "22.56", gradientUnits "userSpaceOnUse" ]
                [ stop [ offset "0", stopColor "#f7931e" ] []
                , stop [ offset "1", stopColor "#f15a24" ] []
                ]
            ]
        , svgHexagon (hex (p 127.575 127.575) 15 125.15 "url(#gradient)")
        ]


calculateCorners : Hexagon -> List Point
calculateCorners hexagon =
    let
        rotate =
            pi / 180 * hexagon.rotation

        radius =
            hexagon.radius

        center =
            hexagon.center

        pointFor t =
            p (radius * (sin t) + center.x) (radius * (cos t) + center.y)

        calculatePoint corner =
            pointFor (pi / 3 * corner + rotate)
    in
        List.map calculatePoint [0..5]


calculateRounding : Point -> Point -> Point -> ( Point, Point, Point )
calculateRounding right current left =
    let
        percentage =
            0.18

        adapt point =
            { point
                | x = ((point.x - current.x) * percentage) + current.x
                , y = ((point.y - current.y) * percentage) + current.y
            }
    in
        ( adapt right, current, adapt left )


drawRounding : String -> ( Point, Point, Point ) -> String
drawRounding prefix ( start, control, end ) =
    String.concat
        [ prefix ++ (String.join "," (List.map toString [ start.x, start.y ]))
        , "Q" ++ (String.join "," (List.map toString [ control.x, control.y, end.x, end.y ]))
        ]


svgHexagon : Hexagon -> Svg msg
svgHexagon hexagon =
    let
        corners =
            calculateCorners hexagon

        toEnd n xs =
            (List.drop n xs) ++ (List.take n xs)

        roundings =
            List.map3 calculateRounding corners (toEnd 1 corners) (toEnd 2 corners)

        sections =
            Maybe.map2 (,) (List.head roundings) (List.tail roundings)
    in
        case sections of
            Just (x, xs) ->
                Svg.path [ d (String.concat ((drawRounding "M" x) :: (List.map (drawRounding "L") xs)))
                         , fill hexagon.fill
                         ]
                         []

            Nothing ->
                text ""
