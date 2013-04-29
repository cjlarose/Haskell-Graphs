module GraphGen (
    list,
    GraphGen.cycle,
    star,
    complete,
    binaryTree,
) where
import Graph (graphFromEdgeList)

list n = graphFromEdgeList [(show v, show (v+1)) | v <- [1..(n-1)]]

cycle n = graphFromEdgeList [(show v, show ((v+1) `mod` n)) | v <- [1..n]]

star n = graphFromEdgeList [("1", (show v)) | v <- [2..n]]

complete n = graphFromEdgeList 
    [((show a), (show b)) | a <- [1..n], b <- [1..n], a /= b]

binaryTree n = graphFromEdgeList []
