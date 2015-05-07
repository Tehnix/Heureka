module Graph where
import qualified Data.Map     as Map
import qualified Data.PSQueue as PQ
import           Data.Maybe   (fromJust)
import Debug.Trace

-- | The edge label is simply a string
type Label = String

-- | The edge weight
type Weight = Float

-- | An edge is connected to two vertices, can have a label and a weight
data Edge v = Edge
    { start :: v
    , end :: v
    , label :: Label
    , weight :: Weight
    } deriving (Eq, Show)

-- and an associated list of edges from that vertex (the values to the key)
type Graph v = Map.Map v [Edge v]


-- | Create a graph edge
edge :: v -> v -> Label -> Weight -> Edge v
edge s e l w = Edge
    { start = s
    , end = e
    , label = l
    , weight = w }

-- | Insert the start vertex of an edge into the key value of a map, and
-- append the edge itself to a list of edges that go out from that vertex
insertEdge :: (Ord v) => Graph v -> Edge v -> Graph v
insertEdge g e = Map.insertWith (++) (start e) [e] g

-- | Construct an empty graph
emptyGraph :: Graph v
emptyGraph = Map.empty

-- | Construct a graph from a list of edges
constructGraph :: (Show v, Ord v) => Graph v -> [Edge v] -> Graph v
constructGraph g [] = g
constructGraph g es = foldl insertEdge g es

-- | Find the edges leading out of a vertex, also called its neighbors
neighbors :: (Ord v) => v -> Graph v -> [Edge v]
neighbors = Map.findWithDefault []

-- | Get the cost that a vertex has been given
cost :: (Ord v) => v -> Map.Map v Float -> Float
cost = Map.findWithDefault 0

-- | A* search algorithm
aStarSearch :: (Ord v, Show v) => Graph v -> v -> v -> (v -> v -> Float) -> (Map.Map v (Maybe v), Map.Map v Float)
aStarSearch graph startVertex goalVertex heuristic =
        search cameFrom frontier costSoFar
    where
        -- Initialization values for the search
        cameFrom = Map.fromList [(startVertex, Nothing)]
        frontier = PQ.fromList [startVertex PQ.:-> 0]
        costSoFar = Map.fromList [(startVertex, 0)]
        -- The search itself
        search from frn costSF | PQ.null frn = (from, costSF)
                               | otherwise   = do
            let current = PQ.key $ fromJust $ PQ.findMin frn
            let (from', frn', costSF') = exploreNeighbors from (PQ.deleteMin frn) costSF current (neighbors current graph)
            search from' frn' costSF'
        -- Explore the neighbors of the current vertex
        exploreNeighbors from frn costSF current [] = (from, frn, costSF)
        exploreNeighbors from frn costSF current (n:ns) = do
            let newCost = cost current costSF + weight n
            let next = end n
            if Map.notMember next costSF || newCost < cost next costSF
                then exploreNeighbors
                        (Map.insert next (Just current) from)
                        (PQ.insert next (newCost + heuristic goalVertex next) frn)
                        (Map.insert next newCost costSF)
                        current
                        ns
                else exploreNeighbors from frn costSF current ns

-- | Find the edge that is between two vertices
edgeBetweenVertices :: (Ord v, Eq v) => Graph v -> v -> v -> Edge v
edgeBetweenVertices graph v1 v2 = head $ filter (\e -> end e == v2) (fromJust $ Map.lookup v1 graph)

-- | Reconstruct the path taken, by walking backwards to the start vertex
reconstructPath :: (Ord v, Eq v) => v -> v -> Map.Map v (Maybe v) -> Graph v -> [Edge v]
reconstructPath startVertex goalVertex cameFrom graph =
        walk goalVertex
    where
        walk current | current == startVertex = []
                     | otherwise = do
            let current' = fromJust.fromJust $ Map.lookup current cameFrom
            edgeBetweenVertices graph current' current : walk current'
