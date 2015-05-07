module RouteFinder where
import qualified Data.Map     as Map
import qualified Data.PSQueue as PQ
import           Data.Maybe   (fromJust)
import           ReadCSV      (parseCityMapCSV)


-- | The edge label is simply a string
type Label = String

-- | The edge weight
type Weight = Int

-- | A vertex is represented as a coordinate
type Vertex = (Int, Int)

-- | An edge is connected to two vertices, can have a label and a weight
data Edge = Edge
    { start :: Vertex
    , end :: Vertex
    , label :: Label
    , weight :: Weight
    } deriving (Eq)

instance Show Edge where
    show (Edge (s1,s2) (e1,e2) l w) = unwords [show s1, show s2, l, show e1, show e2]

-- | A directed graph is represented as a set of starting vertices (the keys)
-- and an associated list of edges from that vertex (the values to the key)
type Graph = Map.Map Vertex [Edge]

-- | Create a graph edge
edge :: Vertex -> Vertex -> Label -> Weight -> Edge
edge s e l w = Edge
    { start = s
    , end = e
    , label = l
    , weight = w }

-- | Construct an empty graph
emptyGraph :: Graph
emptyGraph = Map.empty

-- | Insert the start vertex of an edge into the key value of a map, and
-- append the edge itself to a list of edges that go out from that vertex
insertEdge :: Graph -> Edge -> Graph
insertEdge g e = Map.insertWith (++) (start e) [e] g

-- | Find the edges leading out of a vertex, also called its neighbors
neighbors :: Vertex -> Graph -> [Edge]
neighbors = Map.findWithDefault []

-- | Construct a graph from a list of edges
constructGraph :: Graph -> [Edge] -> Graph
constructGraph g [] = g
constructGraph g (e:es) = foldl insertEdge g es

-- | Convert the CSV data into a list of edges
convertCSVToEdges :: [(Int, Int, Label, Int, Int)] -> [Edge]
convertCSVToEdges [] = []
convertCSVToEdges ((s1,s2,l,e1,e2):es) = edge (s1,s2) (e1,e2) l 1 : convertCSVToEdges es

-- | Get the cost that a vertex has been given
cost :: Vertex -> Map.Map Vertex Int -> Int
cost = Map.findWithDefault 0

-- | Calculate the manhattan distance on a square grid, based on two vertices
heuristic :: Vertex -> Vertex -> Int
heuristic (a1, a2) (b1, b2) = abs (a1 - a2) + (b1 - b2)

-- | A* search algorithm
aStarSearch :: Graph -> Vertex -> Vertex -> (Vertex -> Vertex -> Int) -> (Map.Map Vertex (Maybe Vertex), Map.Map Vertex Int)
aStarSearch graph startVertex goalVertex heuristic =
        search cameFrom frontier costSoFar
    where
        -- Initialization values for the search
        cameFrom :: Map.Map Vertex (Maybe Vertex)
        cameFrom = Map.fromList [(startVertex, Nothing)]
        frontier :: PQ.PSQ Vertex Int
        frontier = PQ.fromList [startVertex PQ.:-> 0]
        costSoFar :: Map.Map Vertex Int
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
                then ( Map.insert next (Just current) from
                     , PQ.insert next (newCost + heuristic goalVertex next) frn
                     , Map.insert next newCost costSF )
                else exploreNeighbors from frn costSF current ns

-- | Find the edge that is between two vertices
edgeBetweenVertices :: Graph -> Vertex -> Vertex -> Edge
edgeBetweenVertices graph v1 v2 = head $ filter (\e -> end e == v2) (fromJust $ Map.lookup v1 graph)

-- | Reconstruct the path taken, by walking backwards to the start vertex
reconstructPath :: Vertex -> Vertex -> Map.Map Vertex (Maybe Vertex) -> Graph -> [Edge]
reconstructPath startVertex goalVertex cameFrom graph =
        walk goalVertex
    where
        walk current | current == startVertex = []
                     | otherwise = do
            let current' = fromJust.fromJust $ Map.lookup current cameFrom
            edgeBetweenVertices graph current' current : walk current'

-- | Constuct a graph from a CSV file
cityMapGraph :: IO Graph
cityMapGraph = do
    csv <- parseCityMapCSV "citymap.txt"
    return $ constructGraph emptyGraph $ convertCSVToEdges csv

-- | Run the A* algorithm on the CityMap Graph
perfromAStarRun :: IO ()
perfromAStarRun = do
    g <- cityMapGraph
    let startVertex = (35, 80) -- Cornor of SktPedersStraede & Larsbjoernsstraede
    let goalVertex = (45, 70) -- Cornor of Studiestraede & Larsbjoernsstraede
    -- Perform the search
    let (cameFrom, costSoFar) = aStarSearch g startVertex goalVertex heuristic
    -- Reconstruct the path taken
    let path = reverse $ reconstructPath startVertex goalVertex cameFrom g
    -- Finally, output it kinda nicely
    putStrLn "Path obtained by running A* on the city map"
    mapM_ print path


main :: IO ()
main = perfromAStarRun
