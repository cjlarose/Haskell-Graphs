module Draw (
    createWindow,
) where

import SOE
import GDraw (getAllPositions)
import Graphics.UI.GLFW (terminate)
import qualified Point (round)
import qualified Data.Graph as Graph

-- -- -- -- -- -- -- -- -- --
--     Window Creation     --
-- -- -- -- -- -- -- -- -- --

-- g w h i t <=> graph width height iterations tweak
createWindow ::
    (RealFrac a, Floating a, Ord a) =>
        Graph.Graph -> Int -> Int -> Int -> a -> IO ()
createWindow g w h i t = do
    win <- openWindow "Chris and Roey's Zany Graph Drawing Window" (w, h)
    resultMap <- (getResultMap g w h i t)
    mapM_ (drawInWindow win) (head resultMap)
    onClose win

getResultMap ::
    (RealFrac a, Floating a, Ord a) =>
        Graph.Graph -> Int -> Int -> Int -> a -> IO [[Graphic]]
getResultMap g w h i t = do
    posList <- getAllPositions g w h i t
    let resultMap = map (\x -> createFrame g i x) posList
    return resultMap

onClose :: Window -> IO ()
onClose w = do
    k <- getKey w
    if k == '\x0' then
        terminate
    else
        onClose w
-- -- -- -- -- -- -- -- -- --
--     Create Circles      --
-- -- -- -- -- -- -- -- -- --
createFrame ::
    (RealFrac a, Floating a, Ord a) =>
        Graph.Graph -> Int -> [(a, a)] -> [Graphic]
createFrame g i list = circles ++ lines
    where circles = map (\x -> createCircle g i x) list
          lines   = getLineGraphics g list

createCircle ::
    (RealFrac a, Floating a, Ord a) =>
        Graph.Graph -> Int -> (a, a) -> Graphic
createCircle g i point = SOE.ellipse (x - r, y - r) (x + r, y + r)
    where (x, y) = Point.round point
          r = 15

--TODO: calculateRadius Will count number of nodes to draw and make the radius of the circles sized accordingly to fit

-- -- -- -- -- -- -- -- -- --
--      Create Lines       --
-- -- -- -- -- -- -- -- -- --
getLineGraphics g ps = map f (getLineEndpoints g ps)
  where
    f (p1, p2) = SOE.line (Point.round p1) (Point.round p2)

getLineEndpoints g ps = map f (Graph.edges g)
  where
    f (u,v) = (ps !! u, ps !! v)
