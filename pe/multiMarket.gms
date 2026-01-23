********************************************************************************
$ontext

   Multi-Market Model with Iso-Elastic Supply and Demand Functions

   GAMS file : multiMarket.gms
   
   @purpose  :
   @author   : W. Britz
   @date     : 16.10.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************

* --- Initialize error catching mechanism
$batinclude ../shared/assert_no_problem.gms init

* --- Declare the sample model

set i "Commodities" / pigs,poultry /;
alias(i,j);


Parameters
    p_supElas(i) "Supply elasticities"   /  pigs 0.5,  poultry 0.3/
    p_demElas(i) "Demand elasticities"   / pigs -0.2, poultry -0.6 /
    p_cnstSup(i) "Constant term in supply function"
    p_cnstDem(i) "Constant term in demand function"
    p_qBase(i)   "Market clearing quantity in base situation"  / pigs 1000, poultry 1000 /
    p_pBase(i)   "Market clearing price in base situation"     / pigs 100, poultry 100 /
    p_subs(i)    "Subsidy"                                     / pigs 0, poultry 0 /;

Variables
    v_sup(i)     "Supply quantity"
    v_dem(i)     "Demand quantity"
    v_p(i)       "Price";

Equations
    e_sup     "Iso-elastic supply equation"
    e_dem     "Iso-elastic demand equation"
    e_mkt     "Market clearing";
    
e_sup(i) ..
    log(v_sup(i)) =E= p_cnstSup(i) + p_supElas(i) * log(v_p(i) + p_subs(i));

e_dem(i) ..
    log(v_dem(i)) =E= p_cnstDem(i) + p_demElas(i) * log(v_p(i));

e_mkt(i) ..
    v_sup(i) =E= v_dem(i);

model m_singleMarket "The simulation model" / e_sup.v_sup
                       e_dem.v_dem
                       e_mkt.v_p    /;

* ----- Check that model is correctly specified -----
*     Uncomment one of the following lines to provoke an error in the model specification test
*p_supElas("pigs") = -0.5;
*p_demElas("poultry") = 0.6;

*     Supply elasticities must be positive
p_problem1D(i) = min(0, p_supElas(i));
$batinclude ../shared/assert_no_problem.gms p_problem1D "Supply elasticities must be positive"

*     Demand elasticities must be negative
p_problem1D(i) = max(0, p_demElas(i)); 
$batinclude ../shared/assert_no_problem.gms p_problem1D "Demand elasticities must be negative"


* --- set start values and calculate constant terms

v_sup.l(i) = p_qBase(i);
v_dem.l(i) = p_qBase(i);
v_p.l(i)   = p_pBase(i);


* --- calculate constant terms from given price, elasticities
*     and quantities

p_cnstSup(i) = log(v_sup.l(i)) - p_supElas(i) * log(v_p.l(i) + p_subs(i));
p_cnstDem(i) = log(v_dem.l(i)) - p_demElas(i) * log(v_p.l(i));


* ----- check if model calibrates by solving with the parameters calculated above -----

*     Uncomment one of the following lines to provoke an error in the calibration test
*p_subs("pigs") = 1;
*p_pBase("poultry") = 200;

solve m_singleMarket using CNS;

*     Check that equilibrium supply matches base supply within tolerance
p_problem1D(i) $ [abs(v_sup.l(i) - p_qBase(i))] = v_sup.l(i) - p_qBase(i);
$batinclude ../shared/assert_no_problem.gms p_problem1D "Calibration error: Equilibrium supply quantities do not match base quantities within tolerance"

*     Check that equilibrium demand matches base demand within tolerance
p_problem1D(i) $ [abs(v_dem.l(i) - p_qBase(i))] = v_dem.l(i) - p_qBase(i);
$batinclude ../shared/assert_no_problem.gms p_problem1D "Calibration error: Equilibrium demand quantities do not match base quantities within tolerance"

*     Check that equilibrium prices match base prices within tolerance
p_problem1D(i) $ [abs(v_p.l(i) - p_pBase(i))] = v_p.l(i) - p_pBase(i);
$batinclude ../shared/assert_no_problem.gms p_problem1D "Calibration error: Equilibrium prices do not match base prices within tolerance"


* --- Store baseline results for comparison

parameter p_res(i,*,*);
p_res(i,"supply","bas") = v_sup.l(i);
p_res(i,"demand","bas") = v_dem.l(i);
p_res(i,"p_sup","bas") = v_p.l(i) + p_subs(i);
p_res(i,"p_dem","bas") = v_p.l(i);



* ----- Check that the model responds correctly to a subsidy on production - an increased supply of the relevant good is expected -----
*     Uncomment the following line to provoke an error in the subsidy response test      
p_supElas("pigs") = -0.05;
*p_demElas("poultry") = 1;

loop(j, 
*   Set a subsidy on commodity j and solve model
    p_subs(j) = 20;
    solve m_singleMarket using CNS;
  
*   Check that supply of commodity j has increased compared to baseline
    p_problem1D(j) = min(0, v_sup.l(j) - p_res(j,"supply","bas"));
    $$batinclude ../shared/assert_no_problem.gms p_problem1D "Error: Supply did not increase after introduction of subsidy"

*   Reset subsidy for commodity j
    p_subs(j) = 0;
);


* --- Run a scenario and display results

p_subs("pigs") = 30;
p_subs("poultry") = 10;

solve m_singleMarket using CNS;

p_res(i,"supply","subs") = v_sup.l(i);
p_res(i,"demand","subs") = v_dem.l(i);
p_res(i,"p_sup","subs") = v_p.l(i) + p_subs(i);
p_res(i,"p_dem","subs") = v_p.l(i);

display p_res;
