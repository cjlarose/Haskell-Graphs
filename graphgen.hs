module GraphGen (
    list,
    cycle',
    star,
    complete,
    binaryTree
) where
import Graph (graphFromEdgeList)

list n = graphFromEdgeList []
cycle' n = graphFromEdgeList []
star n = graphFromEdgeList []
complete n = graphFromEdgeList [((show a), (show b)) | a <- [1..n], b <- [1..n], a /= b]
binaryTree n = graphFromEdgeList []
