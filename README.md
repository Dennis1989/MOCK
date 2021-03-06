# Implementation of MOCK in R

Here we have the first working implementation of the multi objective clustering algorithm MOCK, introduced by 

> Julia Handl and Joshua Knowles. (2007) An evolutionary approach to multiobjective clustering. IEEE Transactions on Evolutionary Computation 11(1):56-7

MOCK offers a great way of clustering data, by optimizing two different objective functions: Overall _deviation_ and _connectivity_. It provides a set of pareto-optimal solutions after one run.

Additionaly an automatic approach of identifying the best solution is implemented.

All cluster solutions and the approximated pareto front can be visualized by using configurable functions.

In order to get the packages just type

`devtools::install_github(repo="Dennis1989/MOCK",subdir="MOCK")`

`devtools::install_github(repo="Dennis1989/MOCK",subdir="PESAII")`
