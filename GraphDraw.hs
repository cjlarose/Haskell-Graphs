module GraphDraw (
    createWindow,
    sierpinskiTriangle,
) where

import Data.Maybe (isJust, fromJust)
import qualified Data.Graph as Graph
import Data.Int (Int64)
import qualified Control.Concurrent.Timer as Timer (oneShotTimer)
import qualified Control.Concurrent.Suspend.Lifted as Delay (msDelay, Delay)
import Graphics.UI.GLFW (terminate)
import SOE
import GraphPhysics (newGraphAnimation, getNextGraph, GraphAnimation, graph, positions)
import qualified Point (round, add)

-- -- -- -- -- -- -- -- -- --
--     Window Creation     --
-- -- -- -- -- -- -- -- -- --

createWindow :: (RealFrac a, Show a, Floating a, Ord a) =>
    GraphAnimation a -> Int -> Int -> Int64 -> Color -> Color -> IO ()
createWindow ga w h delayms nodeColor edgeColor = do
    win <- openWindow "Chris and Roey's Zany Graph Drawing Window" (w, h)
    let center = (w `div` 2, h `div` 2)
    drawGraph win ga center (Delay.msDelay delayms) nodeColor edgeColor
    onClose win
        where center = (w `div` 2, h `div` 2)

drawGraph :: (RealFrac a, Show a, Floating a, Ord a) =>
    Window -> GraphAnimation a -> (Int, Int) -> Delay.Delay -> Color -> Color -> IO ()
drawGraph w ga center delay nodeColor edgeColor = do
    putStrLn (show (positions ga))
    let frame = createFrame center (graph ga) (positions ga) nodeColor edgeColor
    let graphic = overGraphics frame
    let newGraph = getNextGraph ga
    setGraphic w graphic
    if (isJust newGraph)
        then Timer.oneShotTimer
                (drawGraph w (fromJust newGraph) center delay nodeColor edgeColor) delay
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

createFrame :: (RealFrac a) =>
        (Int, Int) -> Graph.Graph -> [(a, a)] -> Color -> Color -> [Graphic]
createFrame center g list nodeColor edgeColor = circles ++ lines
    where
        circles = map (\x -> withColor nodeColor (createCircle x)) list
        lines   = getLineGraphics center g list edgeColor
        createCircle point = SOE.ellipse upperLeft lowerRight
            where upperLeft = Point.add center (x - r, y - r)
                  lowerRight = Point.add center (x + r, y + r)
                  (x, y) = Point.round point
                  r = 15

-- -- -- -- -- -- -- -- -- --
--      Create Lines       --
-- -- -- -- -- -- -- -- -- --

getLineGraphics :: (RealFrac a) =>
        (Int, Int) -> Graph.Graph -> [(a, a)] -> Color -> [Graphic]
getLineGraphics center g ps color =
    map b (map f (Graph.edges g))
        where f (u,v) = (ps !! u, ps !! v)
              b (p1, p2) = (withColor color $ line
                    (Point.add center $ Point.round p1)
                    (Point.add center $ Point.round p2))

-- -- -- -- -- -- -- -- -- --
--     Messing Around      --
-- -- -- -- -- -- -- -- -- --

sierpinskiTriangle w h color =
    runGraphics (
        do openWindow "Sierpinski's Triangle" (w, h)
            >>= \win ->
            sierpinskiTri win 50 300 512 color
            >> onClose win
        )

fillTri :: Window -> Int -> Int -> Int -> Color -> IO ()
fillTri w x y size color
    = drawInWindow w (withColor color
        (polygon [(x,y), (x + size, y), (x, y - size), (x, y)]))

minSize :: Int
minSize = 2

sierpinskiTri :: Window -> Int -> Int -> Int -> Color -> IO ()
sierpinskiTri w x y size color
    = if size <= minSize
        then fillTri w x y size color
        else let size2 = size `div` 2
            in do sierpinskiTri w x y size2 color
                  sierpinskiTri w x (y - size2) size2 color
                  sierpinskiTri w (x + size2) y size2 color
