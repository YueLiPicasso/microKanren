# My Reproduction of microKanren

Reference:
_Jason Hemann and Daniel Friedman (2013) µKanren: A Minimal Functional Core for Relational Programming._ 

The source code contains, among others, microKanren that consists of logic variables, unification, list monad and the four basic goal constructors. 

There are two ways to understand the (operational) semantic of microKanren: 

- __macroscopic semantic:__ we add the macros (`fresh`,`run`,`conde`,etc) and program using these macros (so we are programming in miniKanren). We understand the programs according to the semantic of logic programming (SLD-resolution). 

- __microscopic semantic:__ We refrain from using the macros (`fresh`,`run`,`conde`,etc) when defining and querying about goals, and we see microKanren programs as Scheme programs, and apply the semantic of Scheme.

These two ways resemble our understanding of the physical world, for example, the room temprature is read from a thermometer (macroscopic) but at the atomic level it is the velocity of the air molecules.


The syntactic forms `delay` ,`snooze` and `force` are used to highlight in the source code the techniques of (infinite) stream processing, viz., _delay_ (a.k.a. suspension/continuation), _inverse-eta-delay_ and _force_, respectively.




