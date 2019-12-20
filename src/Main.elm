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
import Random


type alias Life = { position : Vector.Vector2
                    , velocity : Vector.Vector2
                    , size : Float
                    , lifeType : LifeType
                    }
type LifeType = Creature | Graviton

type alias Model =
    { lifes : List Life }


type Msg
    = AddLifes (List Life)
    | Frame Float

randomLife : Random.Generator Life
randomLife = let pos_x = Random.float 0 width
                 pos_y = Random.float 0 height
                 vel_x = Random.float 0 20
                 vel_y = Random.float 0 20
                 size = 30
             in Random.map4 (\x y v_x v_y -> {position = Vector.Vector2 x y,
                                                   velocity = Vector.Vector2 v_x v_y,
                                                   size = size,
                                                   lifeType=Creature}) pos_x pos_y vel_x vel_y
randomGraviton : Random.Generator Life
randomGraviton = Random.map (\l -> {l | lifeType = Graviton, size = 10}) randomLife


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


init : (Model, Cmd Msg)
init = ({lifes=[]}
        , (Random.generate AddLifes (Random.map2 (++)
                                                 (Random.list 10 randomLife)
                                                 (Random.list 10 randomGraviton))))

--{ lifes = [ {position= Vector.fromTuple (100, 100),
--                     size=70,velocity=Vector.fromTuple (4,4)},
--                    {position=Vector.fromTuple(10, 10),
--                    size=70, velocity=Vector.fromTuple(5,2)},
--                    {position=Vector.fromTuple(100, 100),
--                    size=70, velocity=Vector.fromTuple(-6,-4)}
--                    ] }

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
                    AddLifes lifes ->
                        ( { model | lifes = lifes ++ model.lifes}, Cmd.none )

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
  case life.lifeType of
    Creature ->
      shapes
        [fill (Color.hsl 0.5 0.3 0.7)]
        [ circle (Vector.toTuple life.position) life.size ]
    Graviton -> shapes
        [fill (Color.hsl 0.5 0.2 0.2)]
        [ circle (Vector.toTuple life.position) life.size ]
