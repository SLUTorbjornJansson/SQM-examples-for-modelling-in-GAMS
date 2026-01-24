********************************************************************************
$ontext

   Multi-Market Model with Iso-Elastic Supply and Demand Functions

   GAMS file : multiMarket.gms

   @purpose  : Didactic model with examples of various checks
   @author   : W. Britz
   @date     : 16.10.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : Stand-alone

$offtext
********************************************************************************
  $$offlisting

* --- Initialize error catching mechanism
  $$batinclude ../shared/assert_no_problem.gms init

* ------------------------------------------------------------------------------
*
*  Declare the sample model
*
* ------------------------------------------------------------------------------

  set i "Commodities" / pigs,poultry /;
  alias(i,j);

  Parameters
    p_supElas(i) "Supply elasticities"   /  pigs 0.5,  poultry 0.3/
    p_demElas(i) "Demand elasticities"   / pigs -0.2, poultry -0.6 /
    p_cnstSup(i) "Constant term in supply function"
    p_cnstDem(i) "Constant term in demand function"
    p_qBase(i)   "Market clearing quantity in base situation"  / pigs 1000, poultry 1000 /
    p_pBase(i)   "Market clearing price in base situation"     / pigs 100, poultry 100 /
    p_subs(i)    "Subsidy"                                     / pigs 0, poultry 0 /
  ;

  Variables
    v_sup(i)     "Supply quantity"
    v_dem(i)     "Demand quantity"
    v_p(i)       "Price"
  ;

  Equations
    e_sup(i)     "Iso-elastic supply equation"
    e_dem(i)     "Iso-elastic demand equation"
    e_mkt(i)     "Market clearing"
  ;

  e_sup(i) ..
    log(v_sup(i)) =E= p_cnstSup(i) + p_supElas(i) * log(v_p(i) + p_subs(i));

  e_dem(i) ..
    log(v_dem(i)) =E= p_cnstDem(i) + p_demElas(i) * log(v_p(i));

  e_mkt(i) ..
    v_sup(i) =E= v_dem(i);

  model m_MultiMarket "The simulation model" / e_sup.v_sup
                                                e_dem.v_dem
                                                e_mkt.v_p    /;

  m_MultiMarket.limCol = 0;
  m_MultiMarket.limRow = 0;

  if ( execerror, abort "Run-Time error in model definitions, in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation errors in model definitions, in file: %system.fn% , before line: %system.incline%

* ------------------------------------------------------------------------------
*
*   Check that model is correctly specified
*
* ------------------------------------------------------------------------------

* ----- Uncomment one of the following lines to provoke an error in the model specification test
* p_supElas("pigs") = -0.5;
* p_demElas("poultry") = 0.6;
* p_pBase("pigs") = 0;

* --- Supply elasticities must be positive
  p_problem1D(i) = min(0, p_supElas(i));
  $$batinclude ../shared/assert_no_problem.gms p_problem1D "Supply elasticities must be positive"

* --- Demand elasticities must be negative
  p_problem1D(i) = max(0, p_demElas(i));
  $$batinclude ../shared/assert_no_problem.gms p_problem1D "Demand elasticities must be negative"

* --- If there is a base quantity, there must also be a base price, and vice versa
  p_problem1D(i) = 1 $ p_qBase(i)
                 - 1 $ p_pBase(i) ;
  $$batinclude ../shared/assert_no_problem.gms p_problem1D "If there is a base quantity, there must also be a base price"

* --- set start values and calculate constant terms

  v_sup.l(i) = p_qBase(i);
  v_dem.l(i) = p_qBase(i);
  v_p.l(i)   = p_pBase(i);

* --- calculate constant terms from given price, elasticities
*     and quantities

  p_cnstSup(i) = log(v_sup.l(i)) - p_supElas(i) * log(v_p.l(i) + p_subs(i));
  p_cnstDem(i) = log(v_dem.l(i)) - p_demElas(i) * log(v_p.l(i));

  if ( execerror, abort "Compilation errors in benchmarking, in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation errors in benchmarking, in file: %system.fn% , before line: %system.incline%

* ------------------------------------------------------------------------------
*
*   Check that model replicates benchmark
*
* ------------------------------------------------------------------------------

* -----   Uncomment one of the following lines to provoke an error in the calibration test
* p_subs("pigs") = 1;
* p_pBase("poultry") = 200;

  m_MultiMarket.iterlim = 0;
  solve m_MultiMarket using CNS;
  abort $ (m_MultiMarket.numInfes > 0) "Benchmark test resulted in infeasibilities, in file: %system.fn%, line: %system.incline%";
  m_MultiMarket.iterlim = 10000;

* --- Check that equilibrium supply matches base supply within tolerance
  p_problem1D(i) $ [abs(v_sup.l(i) - p_qBase(i)) gt p_problemTol] = v_sup.l(i) - p_qBase(i);
  $$batinclude ../shared/assert_no_problem.gms p_problem1D "Calibration error: Equilibrium supply quantities do not match base quantities within tolerance"

* --- Check that equilibrium demand matches base demand within tolerance
  p_problem1D(i) $ [abs(v_dem.l(i) - p_qBase(i)) gt p_problemTol] = v_dem.l(i) - p_qBase(i);
  $$batinclude ../shared/assert_no_problem.gms p_problem1D "Calibration error: Equilibrium demand quantities do not match base quantities within tolerance"

* --- Check that equilibrium prices match base prices within tolerance
  p_problem1D(i) $ [abs(v_p.l(i) - p_pBase(i)) gt p_problemTol] = v_p.l(i) - p_pBase(i);
  $$batinclude ../shared/assert_no_problem.gms p_problem1D "Calibration error: Equilibrium prices do not match base prices within tolerance"

* --- Store baseline results for comparison

  parameter p_res(i,*,*);
  $$batinclude 'store_res.gms' "'bas'"

  if ( execerror, abort "Compilation errors in benchmark checks and reporting, in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation errors in benchmark checks and reporting, in file: %system.fn% , before line: %system.incline%

* ------------------------------------------------------------------------------
*
*  Check that the model responds correctly to a subsidy on production
*   - an increased supply of the relevant good is expected
*
* ------------------------------------------------------------------------------

* ----- Uncomment the following line to provoke an error in the subsidy response test
* p_supElas("pigs") = -0.05;
* p_demElas("poultry") = 1;

  m_MultiMarket.solprint = 2;
  loop(j,
*    --- Set a subsidy on commodity j and solve model
     p_subs(j) = 20;
     solve m_MultiMarket using CNS;
     abort $ (m_MultiMarket.numInfes > 0) "Test solves with subsidy resulted in infeasibilities, in file: %system.fn%, line: %system.incline%";

*    --- Check that supply of commodity j has increased compared to baseline
      p_problem1D(j) = min(0, v_sup.l(j) - p_res(j,"supply","bas"));
      $$batinclude ../shared/assert_no_problem.gms p_problem1D "Error: Supply did not increase after introduction of subsidy"

      $$batinclude 'store_res.gms' j

*    --- Reset subsidy for commodity j
     p_subs(j) = 0;
  );
  m_MultiMarket.solprint = 1;

  if ( execerror, abort "Run-Time error in subsidy test runs, in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation errors in subsidy test runs, in file: %system.fn% , before line: %system.incline%

* --- Run a scenario and display results

  p_subs("pigs")    = 30;
  p_subs("poultry") = 10;

  solve m_MultiMarket using CNS;
  abort $ (m_MultiMarket.numInfes > 0) "Simualtio  resulted in infeasibilities, in file: %system.fn%, line: %system.incline%";

  $$batinclude 'store_res.gms' "'subs'"

  display p_res;

  if ( execerror, abort "Run-Time error in simulation, in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation errors in simulation, in file: %system.fn% , before line: %system.incline%
