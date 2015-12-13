
EFA stands for Energy Flow Analysis.
Aim of the software is to depict, analyse and optimise energy flow
in complex technical systems with energy storage and energy management
(hybrid cars, energy systems, smart buildings, ..)
in a very generic, highly abstract way.

The Code is based on ideas and principles developed by Philipp Guttenberg
in his PhD-Thesis ("Der Autarke Hybrid am Prüfstand", 2004, TU-München, FZG)
and ongoing career.
The two inactive patent applications
PCT/EP2009/063695, PCT/EP2013/003348 describe the method.

Due to the knowledge of Heinrich Hördegen
(http://www.funktional.info/, Haskell Stammtisch München)
a comprehensive software development in Haskell could be started.
Haskell is a language which has unique and fascinating properties:
high speed, good parallelisation, strong type system.
Code which compiles is almost correct, the few bugs left are easy to find,
rewriting software only in rare cases corrupts the results.

In 2012 we founded the Ingenieurbüro Guttenberg & Hördegen,
tried to mature the method, develop the software and find a business case.
From late summer 2012 Henning Thielemann worked for us as a freelancer
(due to lack of money on our side greatly underpaid but with a lot of effort)
on this project.

After 3 years of self-funded development and lack of business development,
we finally had to give up as a company end of 2014.
We decided to make the software available under the BSD3 license.
The Software consists of:

* vector and matrix operations based on unboxed vectors
* an equation solver, based on unique-logic-tf from Henning Thielemann
* calculation methods to calculate with absolute energy flow or flow change or energy mix
* energy flow graph to show system topology, instant energy flow, cumulated energy flow, sequential energy flow or state flow
* plotting routines for signals and matrices
* energy flow optimisation and analysis routines
* demos and examples

Only one example with some technical relevance is available,
but there are several demos showing different aspects of the software.
The development however is incomplete in many ways:

* the optimisation works only for one storage
* target storage balance in optimisation cannot be achieved exactly due to quantisation effects
* singular states, e.g an inactive storage or connection, can't be taken into account
* the equation can't detect an overdetermination of the equation system
* the software wasn't parallelised yet
* there is no time domain optimisation to account for saturation losses (full or empty storage)

But also the quality of software especially in the younger modules far from perfect.
The main reason:
For commercial reasons we needed a POC (Proof of Concept) as quickly as possible.
We wanted to make a demonstrator (thin slice)
which demonstrates the whole process for technically relevant examples,
before refining and optimising the code.
Unfortunately Haskell's strong typing forced us again and again in a rework,
even though the target seemed so close.
Prototyping in Haskell is really great -- but on a big scale it can be hell.

As far as time allows,
we will keep developing the software on a slow but steady pace.
We herewith encourage everybody and
especially the Haskell community to review the code and to contribute.
Maybe EFA can contribute to the development of a set of Haskell libraries
with technical focus, e.g. like simulation, analysis and control.
We furthermore want to encourage a discussion
how coding big software packages in haskell can be made easier.

Heinrich Hördegen and Philipp Guttenberg
