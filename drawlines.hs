import qualified Data.Graph as Graph
import Graphics.UI.GLFW (terminate)
import qualified SOE (openWindow, drawInWindow, Window, getKey, line)
import qualified Point
import qualified GraphGen

drawLines g ps = do
    w <- SOE.openWindow "I'm Drawing a Circle" (400 , 400)
    let lines = getLineGraphics g ps
    mapM_ (SOE.drawInWindow w) lines
    onClose w

getLineGraphics g ps = map f (getLineEndpoints g ps)
  where
    f (p1, p2) = SOE.line (Point.round p1) (Point.round p2)

getLineEndpoints g ps = map f (Graph.edges g)
  where
    f (u,v) = (ps !! u, ps !! v)

onClose :: SOE.Window -> IO ()
onClose w = do 
    k <- SOE.getKey w
    if k == '\x0' then
        terminate
    else
        onClose w

main = drawLines g pos
  where
    (g,_,_) = GraphGen.complete 5
    pos = [(200, 100), (275, 140), (235, 200), (165, 200), (125, 140)]
