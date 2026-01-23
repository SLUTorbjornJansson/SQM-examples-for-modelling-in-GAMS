********************************************************************************
$ontext

   Simulation Model for Policy Analysis

   GAMS file : SINGLEMARKET.GMS

   @purpose  :
   @author   : W. Britz
   @date     : 16.10.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************

 set i "Commodities" / pigs,poultry /;


 parameters

   p_supElas(i,j) "Supply elasticities"   /  pigs 0.5,  /
   p_demElas(i,j) "Demand elasticities"   / -0.2 /
   p_cnstSup(i) "Constant term in supply function"
   p_cnstDem(i) "Constant term in demand function"
   p_qBase(i)   "Market clearing quantity in base situation"  / 1000 /
   p_pBase(i)   "Market clearing price in base situation"     / 100 /
   p_subs(i)    "Subsidy"                                     /   0 /
 ;
  Variables

    v_sup(i)     "Supply quantity"
    v_dem(i)     "Demand quantity"
    v_p(i)       "Price"

  ;

  Equations

    e_sup     "Iso-elastic supply equation"
    e_dem     "Iso-elastic demand equation"
    e_mkt     "Market clearing"
 ;

 e_sup ..

      log(v_sup) =E= p_cnstSup + p_supElas * log(v_p + p_subs);

 e_dem ..

      log(v_dem) =E= p_cnstDem + p_demElas * log(v_p);

 e_mkt ..

      v_sup =E= v_dem;

 model m_singleMarket / e_sup.v_sup
                       e_dem.v_dem
                       e_mkt.v_p    /;
*
* --- set start values and calculate constant terms
*
 v_sup.l = p_qBase;
 v_dem.l = p_qBase;
 v_p.l   = p_pBase;
*
* --- calculate constant terms from given price, elasticities
*     and quantities
*
 p_cnstSup = log(v_sup.l) - p_supElas * log(v_p.l + p_subs);
 p_cnstDem = log(v_dem.l) - p_demElas * log(v_p.l);
*
* --- check if model calibrates
*
  solve m_singleMarket using CNS;

  parameter p_res(*,*);
  p_res("q,sup","bas") = v_sup.l;
  p_res("q,dem","bas") = v_dem.l;
  p_res("price,sup","bas") = v_p.l + p_subs;
  p_res("price,dem","bas") = v_p.l;

*
* --- remove subsidy and recalculate
*
  p_subs = 20;
  solve m_singleMarket using CNS;
  p_res("q,sup","subs") = v_sup.l;
  p_res("q,dem","subs") = v_dem.l;
  p_res("price,sup","subs") = v_p.l + p_subs;
  p_res("price,dem","subs") = v_p.l;

  display p_res;


