module GraphDraw (
    createWindow,
) where

import Data.Maybe (isJust, fromJust)
import qualified Data.Graph as Graph
import qualified Control.Concurrent.Timer as Timer (oneShotTimer)
import qualified Control.Concurrent.Suspend.Lifted as Delay (msDelay)
import Graphics.UI.GLFW (terminate)
import SOE
import GraphPhysics (newGraphAnimation, getNextGraph, GraphAnimation, graph, positions)
import qualified Point (round, add)

-- -- -- -- -- -- -- -- -- --
--     Window Creation     --
-- -- -- -- -- -- -- -- -- --

-- g w h i t <=> graph width height iterations tweak
{--createWindow ::
    (RealFrac a, Floating a, Ord a) =>
        Graph.Graph -> Int -> Int -> Int -> a -> IO ()--}
createWindow :: (RealFrac a, Show a, Floating a, Ord a) => GraphAnimation a -> Int -> Int -> IO ()
createWindow ga w h = do
    win <- openWindow "Chris and Roey's Zany Graph Drawing Window" (w, h)
    let center = (w `div` 2, h `div` 2)
    drawGraph win ga center
    onClose win

drawGraph :: (RealFrac a, Show a, Floating a, Ord a) => Window -> GraphAnimation a -> (Int, Int) -> IO ()
drawGraph w ga center = do
    putStrLn (show (positions ga))
    let frame = createFrame center (graph ga) (positions ga)
    let graphic = overGraphics frame
    setGraphic w graphic
    {--mapM_ (drawInWindow w) frame--}
    let newGraph = getNextGraph ga
    if (isJust newGraph)
        then 
            Timer.oneShotTimer (drawGraph w (fromJust newGraph) center) (Delay.msDelay 500)
            >> return ()
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
createFrame :: (RealFrac a) => (Int, Int) -> Graph.Graph -> [(a, a)] -> [Graphic]
createFrame center g list = circles ++ lines
    where
        circles = map (\x -> createCircle x) list
        lines   = getLineGraphics center g list
        createCircle point = SOE.ellipse upperLeft lowerRight
            where
                upperLeft = Point.add center (x - r, y - r)
                lowerRight = Point.add center (x + r, y + r)
                (x, y) = Point.round point
                r = 15

--TODO: calculateRadius Will count number of nodes to draw and make the radius of the circles sized accordingly to fit

-- -- -- -- -- -- -- -- -- --
--      Create Lines       --
-- -- -- -- -- -- -- -- -- --
getLineGraphics center g ps = map f (getLineEndpoints g ps)
  where
    f (p1, p2) = SOE.line (Point.add center $ Point.round p1) (Point.add center $ Point.round p2)

getLineEndpoints g ps = map f (Graph.edges g)
  where
    f (u,v) = (ps !! u, ps !! v)
