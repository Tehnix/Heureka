{-# LANGUAGE FlexibleInstances #-}
module RouteFinder where
import ReadCSV (parseCityMapCSV)
import Graph
import Debug.Trace


-- | A vertex is represented as a coordinate
type Vertex = (Int, Int)

-- | Format the output of an Edge with (int, int) vertex a bit nicer
instance Show (Edge Vertex) where
    show (Edge (s1,s2) (e1,e2) l w) = unwords [show s1, show s2, l, show e1, show e2]


-- | Convert the CSV data into a list of edges
convertCSVToEdges :: [(Int, Int, Label, Int, Int)] -> [Edge Vertex]
convertCSVToEdges [] = []
convertCSVToEdges ((s1,s2,l,e1,e2):es) = edge (s1,s2) (e1,e2) l 1 : convertCSVToEdges es

-- | Constuct a graph from a CSV file
cityMapGraph :: IO (Graph Vertex)
cityMapGraph = do
    csv <- parseCityMapCSV "data/citymap.txt"
    return $ constructGraph emptyGraph $ convertCSVToEdges csv

-- | Calculate the manhattan distance on a square grid, based on two vertices
heuristic :: Vertex -> Vertex -> Int
heuristic (a1, a2) (b1, b2) = do
    let h = abs (a1 - a2) + (b1 - b2)
    trace ("h = " ++ show h) h

-- | A directed graph is represented as a set of starting vertices (the keys)
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
