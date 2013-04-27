module Point (
    normal,
    min',
    add,
    sub,
    mul_by_constant
) where

normal (x,y) = sqrt (x^2 + y^2)
min' (ax,ay) (bx,by) | ax < bx = (ax,ay)
                    | ax > bx = (bx,by)
                    | ay < by = (ax,ay)
                    | otherwise = (bx,by)

add (ax,ay) (bx,by) = (ax+bx, ay+by)
sub (ax,ay) (bx,by) = (ax-bx, ay-by)
mul_by_constant (x,y) c = (c * x, c * y)
