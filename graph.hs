import Data.Graph
import System.IO
import Data.List.Split

newGraph :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
newGraph = graphFromEdges []

getEdges (g, _, _) = edges g
nodes (g, vfn, _) = map vfn (vertices g)

addEdge (g, vfn, kfn) k1 k2 = graphFromEdges (n1:n2:old)
    where
        getOrCreateNode key | kfn key == Nothing = (key,key,[])
                            | otherwise = vfn ((\(Just x) -> x) (kfn key))
        (v1n, v1k, v1e) = getOrCreateNode k1
        (v2n, v2k, v2e) = getOrCreateNode k2
        n1 = (v1n, v1k, v2k:v1e)
        n2 = (v2n, v2k, v1k:v2e)
        old = filter (\(_,k,_) -> k /= v1k && k /= v2k) (nodes (g, vfn, kfn))

readGraphFile path = do
    contents <- readFile path
    let lines = endBy "\n" contents
    let p = read (head (tail (splitOn " " (head lines)))) :: Int
    let edges_lines = tail lines
    return (p, edges_lines)
