module Draw (
    initWindow,
) where

import Graphics.UI.GLFW (terminate)
import GDraw
import qualified Data.Graph as Graph
import qualified Point (round)
import SOE

initWindow :: (RealFrac a, Floating a, Ord a) =>
    Graph.Graph -> Int -> Int -> Int -> a -> IO ()
initWindow g w h i t = do
    win <- openWindow "Chris and Roey's Zany Graph Drawing Window" (w, h)
    resultMap <- (getResultMap g w h i t)
    mapM_ (drawInWindow win) (head resultMap)
    onClose win

getResultMap :: (RealFrac a, Floating a, Ord a) =>
    Graph.Graph -> Int -> Int -> Int -> a -> IO [[Graphic]]
getResultMap g w h i t = do
    posList <- getAllPositions g w h i t
    let resultMap = map (\x -> createFrame g i t x) posList
    return resultMap

createFrame :: (RealFrac a, Floating a, Ord a) =>
    Graph.Graph -> Int -> a -> [(a, a)] -> [Graphic]
createFrame g i t list = circles ++ lines
    where circles = map (\x -> createCircle g i t x) list
          lines   = getLineGraphics g list

createCircle :: (RealFrac a, Floating a, Ord a) =>
    Graph.Graph -> Int -> a -> (a, a) -> Graphic
createCircle g i t point = SOE.ellipse (x - r, y - r) (x + r, y + r)
    where (x, y) = Point.round point
          r = 15

--TODO: calculateRadius Will count number of nodes to draw and make the radius of the circles sized accordingly to fit

getLineGraphics g ps = map f (getLineEndpoints g ps)
  where
    f (p1, p2) = SOE.line (Point.round p1) (Point.round p2)

getLineEndpoints g ps = map f (Graph.edges g)
  where
    f (u,v) = (ps !! u, ps !! v)

onClose :: Window -> IO ()
onClose w = do
    k <- getKey w
    if k == '\x0' then
        terminate
    else
        onClose w

--main = initWindow
