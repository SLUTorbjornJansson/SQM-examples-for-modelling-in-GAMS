### This folder contains the sample CGE model (SAM input, balancing, benchmarking, reporting code) with various checks

cge.gms:       A simple single-country CGE without external trade

store_res.gms: Populates a result array with quantity, price and value information from the model results.
               The value information is used to construct a SAM.

samCheck.gms:  Takes a variable v_sam depicting a SAM as input and check if it is balanced.

The code of cge.gms comprises different checks:

(1) Are there empty columns/rows in the SAM?
(2) After using a NLP program to balance the SAM: use samCheck.gms to check if the resulting SAM is balanced
(3) Check for negative transformation elasticities
(4) Check for non-negative respectively positive substitution elasticities
(5) Check for implausible output tax rates
(6) Check for negative share parameters from benchmarking CES functions
(7) Check for negative expenditures by institutions
(8) Check for regularity conditions of demand system parameters
(9) Check for negative bugdet shares by final demanders
(10) Benchmark checks for sub-blocks of the model (factor supply, production functions, income distribution, LES demand system)
(10) Benchmark check for correct initialization of full model with zero iterations
(11) Homogeniety test with 10% increase in numeraire price
(12) Check that quantities do not change if numeraire is reset to unity

After each block, a check for compile and run time errors is performed.
After each model solve, a check for zero number of infeasibilities is triggered.

The code comprises 5 test simulations (10% more capital, 10% more labor, 1% tfp, halving of output taxes, removal of factor taxes).

For each simulation (and the tests 10,11,12), store_res.gms comprises checks:

(a) For non-negativity of selected quantities and prices
(b) That EV calculation returns a zero for benchmark and homogeniety tests
(c) For exhaustion of accounts by institutions (revenues = expenditures)
(d) Construction of SAM and check that it is balanced with samCheck.gms
