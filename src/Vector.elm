module Vector exposing (..)

type alias Vector2 = {x :Float, y: Float}

add : Vector2 -> Vector2 -> Vector2
add a b = Vector2 (a.x+b.x) (a.y+b.y)

mul : Float -> Vector2 -> Vector2
mul c v = Vector2 (c * v.x) (c * v.y)

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


fromTuple : (Float, Float) -> Vector2
fromTuple (x,y) = Vector2 x y

toTuple : Vector2 -> (Float, Float)
toTuple {x,y} = (x,y)
