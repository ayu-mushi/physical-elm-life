module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes, circle)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Vector


type alias Life = { position : Vector.Vector2
                    , velocity : Vector.Vector2
                    , size : Float
                    }

type alias Model =
    { lifes : List Life }


type Msg
    = Frame Float


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }

defaultAnimal : Life
defaultAnimal = {position= Vector.fromTuple (100, 100),
                     size=30,velocity=Vector.fromTuple (0,1)}
init : (Model, Cmd Msg)
init = ({ lifes = [ {position= Vector.fromTuple (100, 100),
                     size=70,velocity=Vector.fromTuple (4,4)},
                    {position=Vector.fromTuple(10, 10),
                    size=70, velocity=Vector.fromTuple(5,2)},
                    {position=Vector.fromTuple(100, 100),
                    size=70, velocity=Vector.fromTuple(-6,-4)}
                    ] }
        , Cmd.none)

lifeUpdate : Life -> Life
lifeUpdate life = let new_pos = Vector.add life.position life.velocity
    in
    moveTo life new_pos

isColliding : Life -> Life -> Bool
isColliding lifeA lifeB = Vector.mag (Vector.sub lifeA.position lifeB.position) <= ((lifeA.size + lifeB.size))

moveTo : Life -> Vector.Vector2 -> Life
moveTo life new_pos =
  if new_pos.x <= 0
    then { life | velocity = Vector.fromTuple(-life.velocity.x, life.velocity.y) }
    else if new_pos.y <= 0 then { life | velocity = Vector.fromTuple(life.velocity.x, -life.velocity.y) }
    else if new_pos.x >= width then { life | velocity = Vector.fromTuple(-life.velocity.x, life.velocity.y) }
    else if new_pos.y >= height then { life | velocity = Vector.fromTuple(life.velocity.x, -life.velocity.y) }
    else {life | position = new_pos}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
                case msg of
                    Frame _ ->
                        ( { model | lifes = List.map lifeUpdate model.lifes }, Cmd.none )

width =
    800


height =
    500


centerX =
    width / 2


centerY =
    height / 2


view : Model -> Html Msg
view model =
  div [] [
    (div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ (Canvas.toHtml
            ( width, height )
            [ style "border" "2px solid rgba(0,0,0,0.1)" ]
            ([ clearScreen] ++ (List.map render model.lifes)))
        ]),
     viewCollision model
   ]


isThereCollision : Model -> Bool
isThereCollision model = (List.map (\x y -> x/=y && isColliding x y) model.lifes) |> List.map (\f -> List.map f model.lifes) |> List.concat |> List.foldl (||) False

viewCollision : Model -> Html msg
viewCollision model =
  if isThereCollision model then
    div [ style "color" "green" ] [ text "Colliding" ]
  else
    div [ style "color" "red" ] [ text "Not Colliding" ]

clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render life =
    shapes
        [fill (Color.hsl 0.5 0.3 0.7)]
        [ circle (Vector.toTuple life.position) life.size ]
-- transform [ translate (Tuple.first life.position) (Tuple.second life.position) , rotate 0]
