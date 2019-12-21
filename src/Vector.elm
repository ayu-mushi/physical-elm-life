module Vector exposing (..)

type alias Vector2 = {x :Float, y: Float}

add : Vector2 -> Vector2 -> Vector2
add a b = Vector2 (a.x+b.x) (a.y+b.y)

zero : Vector2
zero = {x = 0, y = 0}

sum : List Vector2 -> Vector2
sum = List.foldl add zero

mul : Float -> Vector2 -> Vector2
mul c v = Vector2 (c * v.x) (c * v.y)

div  : Float -> Vector2 -> Vector2
div c v = Vector2 ((1/c) * v.x) ((1/c) * v.y)

inverse : Vector2 -> Vector2
inverse a = mul (-1) a

sub : Vector2 -> Vector2 -> Vector2
sub a b = add a (inverse b)

prod : Vector2 -> Vector2 -> Float
prod v w = v.x*w.x + v.y*w.y

square  : Vector2 -> Float
square v = (prod v v)

mag :  Vector2 -> Float
mag v = sqrt (square v)

normalize : Vector2 -> Vector2
normalize v = div (mag v) v

setSizeTo : Float -> Vector2 -> Vector2
setSizeTo c v = v
                |> normalize
                |> mul c

fromTuple : (Float, Float) -> Vector2
fromTuple (x,y) = Vector2 x y

toTuple : Vector2 -> (Float, Float)
toTuple {x,y} = (x,y)
