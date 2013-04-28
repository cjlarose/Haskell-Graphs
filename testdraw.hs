import Graphics.UI.GLFW (terminate)
import SOE

drawCircle :: Int -> Int -> Int -> IO ()
drawCircle x y r = do 
    w <- openWindow "I'm Drawing a Circle" (400 , 400)
    drawInWindow w ( ellipse (x - r , y - r) (x + r , y + r ))
    onClose w

onClose :: Window -> IO ()
onClose w = do 
    k <- getKey w
    if k == '\x0' then
        terminate
    else
        onClose w

main = drawCircle 50 100 100
