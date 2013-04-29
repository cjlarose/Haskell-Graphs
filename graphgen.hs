module GraphGen (
    list,
    GraphGen.cycle,
    star,
    complete,
    binaryTree,
) where
import Graph (graphFromEdgeList)

list n = graphFromEdgeList [(show v, show (v+1)) | v <- [1..(n-1)]]

cycle n = graphFromEdgeList edges
    where edges = map (\(x, y) -> (show (x+1), show (y+1)))
                    [(v, (((v+1) `mod` n))) | v <- [0..n-1]]

star n = graphFromEdgeList [("1", (show v)) | v <- [2..n]]

complete n = graphFromEdgeList
    [((show a), (show b)) | a <- [1..n], b <- [1..n], a /= b]

binaryTree n = graphFromEdgeList (map (\(a,b) -> (show a, show b))
    (concat [neighbors v | v <- [1..n]]))
  where
    neighbors v
        | (2*v) > n = []
        | (2*v + 1) > n = [2*v]
        | otherwise = [2*v, (2*v) + 1]
