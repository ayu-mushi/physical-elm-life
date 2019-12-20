module Main exposing (..)

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
import List.Extra


type alias Life = { position : Vector.Vector2
                    , velocity : Vector.Vector2
                    , size : Float
                    , lifeType : LifeType
                    }
type LifeType = Creature | Graviton {lifespan : Float}

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
randomGraviton = Random.map (\l ->
              {l | lifeType = Graviton {lifespan=1000},
              size = 10})
              randomLife


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
                                                 (Random.list 1 randomLife)
                                                 (Random.list 2 randomGraviton))))
lifeUpdate : Life -> List Life
lifeUpdate life = let new_pos = Vector.add life.position life.velocity
    in
        case life.lifeType of
          (Graviton {lifespan}) -> let new_life = {life | lifeType = Graviton {lifespan=(lifespan - 1)}} in [moveTo new_life new_pos]
          Creature -> [moveTo life new_pos]

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

updateLifeAll model = List.concat <| List.map lifeUpdate model.lifes

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
                case msg of
                    Frame _ ->
                        ( (collisionUpdateAll { model | lifes = (updateLifeAll model) }), Cmd.none )
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
isThereCollision model =
  (List.map (\x y -> x/=y && isColliding x y) model.lifes)
    |> List.map (\f -> List.map f model.lifes)
    |> List.concat
    |> List.foldl (||) False

whenCollide : Life -> Life -> List Life
whenCollide l m = case l.lifeType of
                    Graviton {lifespan}-> [{l | lifeType = Creature}]
                    Creature -> [{l | lifeType = Graviton {lifespan =100}}]

collisionUpdate : Life -> Model -> List Life
collisionUpdate life model =
  (List.map (\y -> if isColliding life y then whenCollide life y else [life]) model.lifes)
    |> List.concat

comb : List a -> List (a,a)
comb l = case l of
  [] -> []
  (x::xs) -> List.map (\y -> (x,y)) xs ++ comb xs

combWith : (a -> a -> b) -> List a -> List b
combWith f l = case l of
  [] -> []
  (x::xs) -> List.map (\y -> f x y) xs ++ combWith f xs

  -- isColliding で返ってきたxを使って他との当たり判定を続行する

combAccumWith : (Life -> Life -> (Life, List Life)) -> List Life -> List (List Life)
combAccumWith f l = case l of
  [] -> []
  (x::xs) -> (((\(v,w)->[v]::w)) (List.Extra.mapAccuml (\x_ y_ -> f x_ y_) x xs))
              ++ combAccumWith f xs


collisionUpdateAll_ : Model -> List Life
collisionUpdateAll_ model = model.lifes
  |> List.map (\l -> collisionUpdate l model)
  |> List.concat

-- これだとどんどんクローンが増えていってしまう
collisionUpdateAll : Model -> Model
collisionUpdateAll model =
  let new_lifes = List.concat <| combWith (\x y -> if isColliding x y then whenCollide x y ++ whenCollide y x else [x,y]) model.lifes
  in {model | lifes=new_lifes}

howManyCollision : Model -> Int
howManyCollision model =
  (List.map (\x y -> if(x/=y && isColliding x y) then 1 else 0) model.lifes)
  |> List.map (\f -> List.map f model.lifes)
  |> List.concat
  |> List.foldl (+) 0


viewCollision : Model -> Html msg
viewCollision model =
  if isThereCollision model then
    div [ style "color" "green" ] [ text <| "Colliding: " ++ String.fromInt(howManyCollision model) ]
  else
    div [ style "color" "red" ] [ text "Not Colliding" ]

clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : Life -> Canvas.Renderable
render life =
  case life.lifeType of
    Creature ->
      shapes
        [fill (Color.hsl 0.5 0.3 0.7)]
        [ circle (Vector.toTuple life.position) life.size ]
    Graviton {lifespan}-> shapes
        [fill (Color.hsl 0.5 1 0.2)]
        [ circle (Vector.toTuple life.position) life.size ]
