module GDraw (
    repulsiveForce,
    attractiveForce,
) where
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Point

tweak = 5

kfun :: Floating a => Graph.Graph -> Int -> Int -> a
kfun g w l = (sqrt (x/y)) * (fromIntegral tweak)
    where x = fromIntegral (w*l)
          y = fromIntegral (length (Graph.vertices g))

fa :: Floating a => Graph.Graph -> Int -> Int -> a -> a
fa g w l z = (z^2) / k
    where k = kfun g w l

fr :: Floating a => Graph.Graph -> Int -> Int -> a -> a
fr g w l z = (k^2) / z
    where k = kfun g w l

repulsiveForce :: (Ord a, Floating a) => Graph.Graph -> [(a,a)] -> Int -> Int -> [(a,a)]
repulsiveForce g ps w l = map disp vs
    where vs = zip (Graph.vertices g) ps
          disp (v,vpos) = Point.sum [disp' f vpos upos | (u,upos) <- vs, u /= v]
          f norm = fr g w l norm

disp' f vpos upos = Point.mul_by_constant delta ((f norm) / norm)
  where delta = Point.sub vpos upos
        norm = max (Point.normal delta) 0.01

edgeMap g = Map.fromListWith (++) edge_list
    where edge_list = map (\(a,b) -> (a,[b])) (Graph.edges g)

edgeMapRev g = Map.fromListWith (++) edge_list
    where edge_list = map ((\(a,b) -> (a,[b])).(\(a,b) -> (b,a))) (Graph.edges g)

attractiveForce :: (Ord a, Floating a) => Graph.Graph -> [(a,a)] -> Int -> Int -> [(a,a)]
attractiveForce g ps w l = map disp vs
    where vs = zip (Graph.vertices g) ps
          e = edgeMap g
          e' = edgeMapRev g
          adjacent v m | Map.lookup v m == Nothing = []
                       | otherwise = map (\x -> (x, ps !! x)) (fromJust (Map.lookup v m))
          adjacentFrom v = adjacent v e
          adjacentTo v = adjacent v e'
          disp v = Point.sub (sumv adjacentTo v) (sumv adjacentFrom v)
          f norm = fa g w l norm
          sumv fn (v,vpos) = Point.sum [disp' f vpos upos | (u,upos) <- fn v]
