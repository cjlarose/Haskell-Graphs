module GDraw (
    repulsiveForce,
    attractiveForce,
    positionNodes,
) where
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified Data.List as List
import Data.Maybe (fromJust)
import qualified Point

kfun :: Floating a => Graph.Graph -> Int -> Int -> a -> a
kfun g w l tweak = (sqrt (x/y)) * tweak
  where 
    x = fromIntegral (w*l)
    y = fromIntegral (length (Graph.vertices g))

fa :: Floating a => Graph.Graph -> Int -> Int -> a -> a -> a
fa g w l z tweak = (z^2) / k
  where 
    k = kfun g w l tweak

fr :: Floating a => Graph.Graph -> Int -> Int -> a -> a -> a
fr g w l z tweak = (k^2) / z
  where 
    k = kfun g w l tweak

disp' f vpos upos = Point.scale delta ((f norm) / norm)
  where 
    delta = Point.sub vpos upos
    norm = max (Point.normal delta) 0.01

repulsiveForce :: (Ord a, Floating a) => Graph.Graph -> [(a,a)] -> Int -> Int -> a -> [(a,a)]
repulsiveForce g ps w l tweak = map disp vs
  where 
    vs = zip (Graph.vertices g) ps
    disp (v,vpos) = Point.sum [disp' f vpos upos | (u,upos) <- vs, u /= v]
    f norm = fr g w l norm tweak

edgeMap g = Map.fromListWith (++) edge_list
  where 
    edge_list = map (\(a,b) -> (a,[b])) (Graph.edges g)

edgeMapRev g = Map.fromListWith (++) edge_list
  where 
    edge_list = map ((\(a,b) -> (a,[b])).(\(a,b) -> (b,a))) (Graph.edges g)

attractiveForce :: (Ord a, Floating a) => Graph.Graph -> [(a,a)] -> Int -> Int -> a -> [(a,a)]
attractiveForce g ps w l tweak = map disp vs
  where 
    vs = zip (Graph.vertices g) ps
    e = edgeMap g
    e' = edgeMapRev g
    adjacent v m 
        | Map.lookup v m == Nothing = []
        | otherwise = map (\x -> (x, ps !! x)) (fromJust (Map.lookup v m))
    adjacentFrom v = adjacent v e
    adjacentTo v = adjacent v e'
    disp v = Point.sub (sumv adjacentTo v) (sumv adjacentFrom v)
    f norm = fa g w l norm tweak
    sumv fn (v,vpos) = Point.sum [disp' f vpos upos | (u,upos) <- fn v]

positionNodes :: (Ord a, Floating a) => Graph.Graph -> [(a,a)] -> [(a,a)] -> [(a,a)] -> Int -> Int -> a -> [(a,a)]
positionNodes g pos rdisp adisp w l temp = zipWith3 repo pos rdisp adisp
  where 
    repo vpos r a = fitInCanvas (Point.add vpos (Point.scale dispv (temp/(Point.normal dispv))))
      where 
        dispv = Point.add r a
    fitInCanvas (x,y) = (min (w'/2) (max (-w'/2) x), min (l'/2) (max (-l'/2) y))
    w' = fromIntegral w
    l' = fromIntegral l

randomList :: Int -> Random.StdGen -> [Int]
randomList n = take n . List.unfoldr (Just . Random.random)

randomPos p w l = do
    seed1 <- Random.newStdGen
    seed2 <- Random.newStdGen
    let l1 = randomList p seed1
    let l2 = randomList p seed2
    let ps = zipWith (\x y -> (x `mod` w, y `mod` l)) l1 l2
    return ps

temperatures :: (Floating a) => Int -> Int -> [a]
temperatures init len = map f [0..len-1]
  where
    k = (fromIntegral init) / (fromIntegral len)
    f x = (fromIntegral init) - ((fromIntegral x) * k)

{--
allPositions g w l iters tweak = foldl f initialPositions temps
  where
    initialPositions = randomPos (length (Graph.vertices g)) w l
    temps = temperatures ((fromIntegreal w) / 10) iters
gdraw :: (Floating a) => Graph.Graph -> Int -> Int -> Int -> a -> [[(a,a)]]
gdraw g w l iters tweak = 
--}
