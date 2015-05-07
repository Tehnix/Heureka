Heureka
===
In the exercise one is to design, implement and test a software system which applies heuristic search methods to

1. Route planning in a city map
2. Logical deduction in propositional logic on clause form


Route Finding
===
In the first part, the task is to design, implement and test a program for calculating a good (car/bicycle) travel route between two given street crossings in a city map, taking into account one-way streets.

Try with an A* algorithm and/or a RBFS algorithm, introducing an appropriate heuristics for guiding the search.

The data can be found in `data/citymap.txt`, and is in the format shown below,

```haskell
    10 70 Vestervoldgade 20 50
    20 50 Vestervoldgade 10 70
    10 70 SktPedersStraede 35 80
```

If we start at the corner of SktPedersStraede & Larsbjoernsstraede and end at the corner of Studiestraede & Larsbjoernsstraede, then manually going through the route, one can come up with the path,

```haskell
    35 80 SktPedersStraede 50 90
    50 90 LarslejStraede 35 120
    35 120 Noerrevoldgade 25 100
    25 100 Noerrevoldgade 10 70
    10 70 Vestervoldgade 20 50
    20 50 Studiestraede 45 70
```

If we run it through Google Maps, we get,

![screen shot 2015-05-07 at 23 19 12](https://cloud.githubusercontent.com/assets/1189998/7526148/ac50937a-f50f-11e4-8bf7-614c7a84d11b.png)

which, in our dataset is impossible,

```haskell
    35 80 SktPedersStraede 50 90
    50 90 LarslejStraede 35 120
    35 120 Noerrevoldgade 60 150
    60 150 Noerregade 65 110
    65 110 Noerregade 65 100
    65 100 Noerregade 70 85
    -- wrong direction !!
    45 70 Studiestraede 70 85
```

The alternate route though is the same as the manually calculated.


### Heuristics
With the given dataset, and the implemented A* algorithm in `Graph.hs`, we can try to optimize our heuristics function. First off, we try a rather simple one,

```haskell
    -- | Calculate the manhattan distance on a square grid, based on two vertices
    heuristic :: Vertex -> Vertex -> Int
    heuristic (a1, a2) (b1, b2) = abs (a1 - a2) + (b1 - b2)
```

we get the following path,

```haskell
    Path obtained by running A* on the city map
    35 80 SktPedersStraede 50 90
    50 90 LarslejStraede 35 120
    35 120 Noerrevoldgade 60 150
    60 150 Noerregade 65 110
    65 110 Noerregade 65 100
    65 100 Noerregade 70 85
    70 85 Noerregade 80 70
    80 70 Vestergade 55 55
    55 55 Vestergade 35 35
    35 35 Vestervoldgade 20 50
    20 50 Studiestraede 45 70
```



Inference Engine for Propositional Logic
===
The second part is a construction of a proof system for propositional logic in clausal form. Optimally, this part will re-use the heuristic search components developed in the first part.

Basically the prover is to construct the proof as a refutation proof.
