import Data.Graph

newGraph p = buildG (0, p) []

addEdge g v1 v2 = buildG (0, p) ((v1,v2):(v2,v1):(edges g))
    where p = length (vertices g)

getEdges = edges 
