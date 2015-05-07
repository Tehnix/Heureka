Heureka
===

In the exercise one is to design, implement and test a software system which applies heuristic search methods to

1. Route planning in a city map
2. Logical deduction in propositional logic on clause form


## Route Finding
In the first part, the task is to design, implement and test a program for calculating a good (car/bicycle) travel route between two given street crossings in a city map, taking into account one-way streets.

The data can be found in `data/citymap.txt`, and is in the format shown below,

<pre>
    10 70 Vestervoldgade 20 50
    20 50 Vestervoldgade 10 70
    10 70 SktPedersStraede 35 80
</pre>


## Inference Engine for Propositional Logic
The second part is a construction of a proof system for propositional logic in clausal form. Optimally, this part will re-use the heuristic search components developed in the first part.

Basically the prover is to construct the proof as a refutation proof.
