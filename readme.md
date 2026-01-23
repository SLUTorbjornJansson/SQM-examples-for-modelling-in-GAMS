# SQM in economic models in GAMS
A set of three different economic models implemented in GAMS with examples of tests and checks.

## Partial equilibrium
File: pe/multiMarket.gms

A partial equilibrium model with two commodities with synthetic price and quantity. The model contains examples of assertion points (?) for valid input data, recovery of the calibration point, and economically sensible response of the calibrated model. By uncommenting indicated lines the model can be corrupted and the errors triggered. The assertion points are indicated by comments enclosed starting in five dashes, "-----".

## Computable General Equilibrium
File: cge/cge.gms

A CGE model with SAM input, balancing, benchmarking, and reporting code. The code implements various checks.

## A single farm model
File: farm/?.gms

