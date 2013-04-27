module Graph (
    graphFromEdgeList,
    graphFromMap,
    readGraphFile
) where
import Data.Graph
import System.IO
import Data.List.Split (splitOn, endBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

type LabeledGraph = (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)

{--
newGraph :: LabeledGraph
newGraph = graphFromEdges []

getEdges (g, _, _) = edges g
nodes (g, vfn, _) = map vfn (vertices g)

addEdge (g, vfn, kfn) k1 k2 = graphFromEdges (n1:n2:old)
    where
        getOrCreateNode key | kfn key == Nothing = (key,key,[])
                            | otherwise = vfn (fromJust (kfn key))
        (v1n, v1k, v1e) = getOrCreateNode k1
        (v2n, v2k, v2e) = getOrCreateNode k2
        n1 = (v1n, v1k, v2k:v1e)
        n2 = (v2n, v2k, v1k:v2e)
        old = filter (\(_,k,_) -> k /= v1k && k /= v2k) (nodes (g, vfn, kfn))
--}

graphFromEdgeList :: [(String, String)] -> LabeledGraph
graphFromEdgeList edges = graphFromMap (Map.union map1 map2)
    where 
        edge_list = map (\(a, b) -> (a, [b])) edges
        map1 = Map.fromListWith (++) edge_list
        orphans = Set.difference (Set.fromList (map snd edges)) (Set.fromList (map fst edges))
        orphan_edges = map (\x -> (x, [])) (Set.toList orphans)
        map2 = Map.fromList orphan_edges

graphFromMap m = graphFromEdges (map (\(k,v) -> (k,k,v)) (Map.toList m))

readGraphFile path = do
    contents <- readFile path
    let lines = endBy "\n" contents
    let p = read (head (tail (splitOn " " (head lines)))) :: Int
    let edge_lines = tail lines
    let edge_list = map ((\[a,b] -> (a,b)).(splitOn " ")) edge_lines
    let graph = graphFromEdgeList edge_list
    return graph
