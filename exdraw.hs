import qualified GraphGen
import qualified Draw
import qualified GDraw

main = do
    let (g,_,_) = GraphGen.cycle 8
    ga <- GDraw.newGraphAnimation g 600 400 10 0.5
    Draw.createWindow ga 600 400 
    return ()
