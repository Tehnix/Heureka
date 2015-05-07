{-# LANGUAGE FlexibleInstances #-}
module RouteFinder where
import ReadCSV (parseCityMapCSV)
import Graph


-- | A vertex is represented as a coordinate
type Vertex = (Int, Int)

-- | Format the output of an Edge with (int, int) vertex a bit nicer
instance {-# OVERLAPPING #-} Show (Edge Vertex) where
    show (Edge (s1,s2) (e1,e2) l w) = unwords [show s1, show s2, l, show e1, show e2]


-- | Convert the CSV data into a list of edges
convertCSVToEdges :: [(Int, Int, Label, Int, Int)] -> [Edge Vertex]
convertCSVToEdges [] = []
convertCSVToEdges ((s1,s2,l,e1,e2):es) = edge (s1,s2) (e1,e2) l (sqrt $ fromIntegral $ (s1-e1)^2 + (s2-e2)^2) : convertCSVToEdges es

-- | Constuct a graph from a CSV file
cityMapGraph :: IO (Graph Vertex)
cityMapGraph = do
    csv <- parseCityMapCSV "data/citymap.txt"
    return $ constructGraph emptyGraph $ convertCSVToEdges csv

-- | Calculate the distance between two vertices
heuristic :: Vertex -> Vertex -> Float
heuristic (a1, a2) (b1, b2) = sqrt $ fromIntegral $ (b1-a1)^2 + (b2-a2)^2

-- | A directed graph is represented as a set of starting vertices (the keys)
-- | Run the A* algorithm on the CityMap Graph
perfromAStarRun :: IO ()
perfromAStarRun = do
    g <- cityMapGraph
    let startVertex = (35, 80) -- Corner of SktPedersStraede & Larsbjoernsstraede
    let goalVertex = (45, 70) -- Corner of Studiestraede & Larsbjoernsstraede
    -- Perform the search
    let (cameFrom, costSoFar) = aStarSearch g startVertex goalVertex heuristic
    -- Reconstruct the path taken
    let path = reverse $ reconstructPath startVertex goalVertex cameFrom g
    -- Finally, output it kinda nicely
    putStrLn "Path obtained by running A* on the city map"
    mapM_ print path

main :: IO ()
main = perfromAStarRun
