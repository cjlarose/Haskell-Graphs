module Draw (
    createWindow,
) where

import SOE
import GDraw (GraphAnimation, getNextGraph, width, height)
import Graphics.UI.GLFW (terminate)
import qualified Point (round)
import qualified Data.Graph as Graph
import Data.Maybe (isJust, fromJust)

-- -- -- -- -- -- -- -- -- --
--     Window Creation     --
-- -- -- -- -- -- -- -- -- --

-- g w h i t <=> graph width height iterations tweak
{--createWindow ::
    (RealFrac a, Floating a, Ord a) =>
        Graph.Graph -> Int -> Int -> Int -> a -> IO ()--}
createWindow :: GraphAnimation a -> IO ()
createWindow ga = do
    win <- openWindow "Chris and Roey's Zany Graph Drawing Window" (width ga, height ga)
    drawGraph ga
    onClose win

drawGraph :: GraphAnimation a -> IO ()
drawGraph ga = do
    let newGraph = getNextGraph ga
    if (isJust newGraph)
        then drawGraph (fromJust newGraph)
        else return ()
    return ()

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
createFrame :: (RealFrac a) => Graph.Graph -> [(a, a)] -> [Graphic]
createFrame g list = circles ++ lines
    where
        circles = map (\x -> createCircle x) list
        lines   = getLineGraphics g list
        createCircle point = SOE.ellipse (x - r, y - r) (x + r, y + r)
            where
                (x, y) = Point.round point
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
