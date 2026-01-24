### This folder contains a simple 2-product multi-market model and related result reporting

multiMarket.gms: Core code with data input, model definition, benchmarking and some solves

store_res.gms: Populates a result array with quantities and prices from the model results.

The code of multiMarket.gms comprises different checks:

(1) Check for positive supply and negative demand elasticity
(2) Check that each non-empty market has both quantity and price information
(3) Benchmark check for no infeasibilities with zero iterations
(4) Benchmark check of supply, demand and price variables fitting the raw data
(4) Market intelligence test: check that introducing a subsidy for a product increases its output

After each block, a check for compile and run time errors is performed.
After each model solve, a check for zero number of infeasibilities is triggered.

The code comprises 5 test simulations (10% more capital, 10% more labor, 1% tfp, halving of output taxes, removal of factor taxes).

For each simulation and the benchmark solte, store_res.gms comprises checks for:

(a) Non-negativity of quantities and prices
(b) equality of supply and demand
