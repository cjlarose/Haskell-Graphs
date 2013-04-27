import Data.Graph
import System.IO
import Data.List.Split

newGraph p = buildG (0, p) []

addEdge g v1 v2 = buildG (0, p) ((v1,v2):(v2,v1):(edges g))
    where p = length (vertices g)

getEdges = edges 


readGraphFile path = do
    contents <- readFile path
    let lines = endBy "\n" contents
    let p = read (head (tail (splitOn " " (head lines)))) :: Int
    let edges_lines = tail lines
    return (p, edges_lines)
