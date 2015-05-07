Heureka
===

In the exercise one is to design, implement and test a software system which applies heuristic search methods to

1. Route planning in a city map
2. Logical deduction in propositional logic on clause form


## Route Finding
In the first part, the task is to design, implement and test a program for calculating a good (car/bicycle) travel route between two given street crossings in a city map, taking into account one-way streets.

Try with an A* algorithm and/or a RBFS algorithm, introducing an appropriate heuristics for guiding the search.

The data can be found in `data/citymap.txt`, and is in the format shown below,

<pre>
    10 70 Vestervoldgade 20 50
    20 50 Vestervoldgade 10 70
    10 70 SktPedersStraede 35 80
</pre>

If we start at the cornor of SktPedersStraede & Larsbjoernsstraede and end at the cornor of Studiestraede & Larsbjoernsstraede, then manually going through the route, one can come up with the path,

<pre>
    35 80 SktPedersStraede 50 90
    50 90 LarslejStraede 35 120
    35 120 Noerrevoldgade 25 100
    25 100 Noerrevoldgade 10 70
    10 70 Vestervoldgade 20 50
    20 50 Studiestraede 45 70
</pre>

If we run it through Google Maps, we get,

![screen shot 2015-05-07 at 23 19 12](https://cloud.githubusercontent.com/assets/1189998/7526148/ac50937a-f50f-11e4-8bf7-614c7a84d11b.png)

## Inference Engine for Propositional Logic
The second part is a construction of a proof system for propositional logic in clausal form. Optimally, this part will re-use the heuristic search components developed in the first part.

Basically the prover is to construct the proof as a refutation proof.
