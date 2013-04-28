module GDraw (
    repulsiveForce,
) where
import qualified Data.Graph as Graph
import qualified Point

{--kfun :: Floating a => Graph.Graph -> Int -> Int -> a--}
kfun g w l = (sqrt (x/y)) * 5
    where x = fromIntegral (w*l)
          y = fromIntegral (length (Graph.vertices g))

fa g w l z = (z^2) / k
    where k = kfun g w l

{--fr :: Floating a => Graph.Graph -> Int -> Int -> a -> a--}
fr g w l z = (k^2) / z
    where k = kfun g w l

{--repulsiveForce :: (Ord a, Floating a) => Graph.Graph -> [(a,a)] -> Int -> Int -> [(a,a)]--}
repulsiveForce g ps w l = map disp vs
    where vs = zip (Graph.vertices g) ps
          disp (v,vpos) = Point.sum [disp' (v,vpos) (u,upos) | (u,upos) <- vs, u /= v]
          disp' (v,vpos) (u,upos) = Point.mul_by_constant delta (f / norm)
              where delta = Point.sub vpos upos
                    norm = max (Point.normal delta) 0.01
                    f = fr g w l norm
