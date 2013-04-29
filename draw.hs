module Draw (
    initWindow,
) where

import Graphics.UI.GLFW (terminate)
import SOE

initWindow :: IO ()
initWindow = openWindow "TEST" (640, 480) >>= onClose

--drawCircle :: Int -> Int -> Int -> Graphic
--drawCircle x y r = drawInWindow ( ellipse (x - r , y - r) (x + r , y + r ))

onClose :: Window -> IO ()
onClose w = do
    k <- getKey w
    if k == '\x0' then
        terminate
    else
        onClose w

main = initWindow
