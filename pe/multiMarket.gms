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

 set i "Commodities" / pigs,poultry /;


 parameters

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

    e_sup     "Iso-elastic supply equation"
    e_dem     "Iso-elastic demand equation"
    e_mkt     "Market clearing"
 ;

 e_sup(i) ..

      log(v_sup(i)) =E= p_cnstSup(i) + p_supElas(i) * log(v_p(i) + p_subs(i));

 e_dem(i) ..

      log(v_dem(i)) =E= p_cnstDem(i) + p_demElas(i) * log(v_p(i));

 e_mkt(i) ..
      v_sup(i) =E= v_dem(i);

 model m_singleMarket / e_sup.v_sup
                       e_dem.v_dem
                       e_mkt.v_p    /;
*
* --- set start values and calculate constant terms
*
 v_sup.l(i) = p_qBase(i);
 v_dem.l(i) = p_qBase(i);
 v_p.l(i)   = p_pBase(i);
*
* --- calculate constant terms from given price, elasticities
*     and quantities
*
 p_cnstSup(i) = log(v_sup.l(i)) - p_supElas(i) * log(v_p.l(i) + p_subs(i));
 p_cnstDem(i) = log(v_dem.l(i)) - p_demElas(i) * log(v_p.l(i));
*
* --- check if model calibrates
*
  solve m_singleMarket using CNS;

  parameter p_res(i,*,*);
  p_res(i,"supply","bas") = v_sup.l(i);
  p_res(i,"demand","bas") = v_dem.l(i);
  p_res(i,"p_sup","bas") = v_p.l(i) + p_subs(i);
  p_res(i,"p_dem","bas") = v_p.l(i);

*
* --- remove subsidy and recalculate
*
  p_subs("pigs") = 20;
  solve m_singleMarket using CNS;
  p_res(i,"supply","subs") = v_sup.l(i);
  p_res(i,"demand","subs") = v_dem.l(i);
  p_res(i,"p_sup","subs") = v_p.l(i) + p_subs(i);
  p_res(i,"p_dem","subs") = v_p.l(i);

  display p_res;


