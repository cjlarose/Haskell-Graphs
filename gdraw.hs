module GDraw (
    repulsiveForce,
) where
import qualified Data.Graph as Graph
import qualified Point

repulsiveForce :: Floating a => Graph.Graph -> [(a,a)] -> Int -> Int -> [(a,a)]
repulsiveForce g ps w l = map disp vs
where vs = zip (Graph.vertices g) ps
      disp (v,vpos) = Point.sum [disp' (v,vpos) (u,upos) | (u,upos) <- vs, u /= v]
      disp' (v,vpos) (u,upos) = Point.mul_by_constant delta (1 / norm)
      where delta = Point.sub vpos upos
            norm = Point.normal delta
