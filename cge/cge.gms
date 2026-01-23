********************************************************************************
$ontext

   Simulation Models for Policy Analysis

   GAMS file : CGE.GMS

   @purpose  : Didactic small, closed economy model, mnemonics
               as in ENVISAGE / CGEBOX

   @author   : Wolfgang Britz
   @date     : 22.03.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************
$offlisting

*---------------------------------------------------------------------------------------------------
*
*
*     (I) Set definitions
*
*
*---------------------------------------------------------------------------------------------------

  SET r "Regions"     / r1     "MyCountry"   /;

  SET SIMs(*) "Counterfactual scenarios run";

*
* --- sets describring the structur of the SAM
*

  SET set_s "Sectors"     / S_Agr "Agriculture"
                            S_Ind  "Industry"
                            S_Ser  "Services"    /;

  set set_c "Commodities" / C_Agr  "Agriculture"
                            C_Ind  "Industry"
                            C_Ser  "Services"    /;

  set set_f "Factors" / cap  "capital"
                        lab  "labor"
                         /;

  SET set_i "Institutions"  / Hou "Household"
                              Gov "Government"
                              Inv "Investments and savings"
                            /;

  SET Cols "Columns of the SAM" /
        set.set_s
        set.set_c
        set.set_f
        set.set_i
        Revenue
  /;


  SET BCols(cols) "Columns to balance";
  BCols(Cols)         = YES;
  BCols("Revenue")    = NO;

  SET Rows "Rows of the SAM" /
        set.set_s
        set.set_c
        set.set_f
        set.set_i
        Expend
  /;

  SET BRows(Rows) "Rows to balance";
  BRows(Rows)         = YES;
  BRows("Expend")     = NO;

  set aa(cols) "All demand agents" / set.set_s,set.set_i /;
  SET s(aa) "Sectors"     / set.set_s /;
  alias(s,s1,a);

  set fdn(aa) / set.set_i /;
  alias(fdn,fdn1);

  set c(rows) "Commodities" / set.set_c /;
  alias(c,c1);

  set f(rows) "Factors"     / set.set_f /;
  alias(f,f1);

  set tax / s "Sectors",f "factors" /;
  set dem / set.set_i /;

  set s_to_c(s,c) "Connection between sector and commodities"
                      / S_AGR.C_AGR,
                        S_IND.C_IND,
                        S_Ser.C_Ser          /;

  $$if not errorfree $abort Compilation errors in set definitions, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*
* (II) Data input
*
*
*---------------------------------------------------------------------------------------------------


* TABLE SAM
*
*              S_Agr  S_Ind  S_SER   C_AGR  C_IND  C_SER    Lab     Cap      Hou       Gov       Inv   Row  Revenue
* r1.S_Agr
* r1.S_Ind                              Domestic sales                                 Exp.         Exports
* r1.S_Ser                                                                           Subsidies
*
* r1.C_Agr
* r1.C_Ind          Intermed.                                             Household    Gov_sam.l    Investments
* r1.C_Ser           Demand                                                     Consumption
*
* r1.Lab           Labor demand
* r1.Cap         Capital demand
*
* r1.Hou                                                    Factor enum.
* r1.Gov          Indirect taxes          Import tariffs    Factor taxes  Dir. Tax
* r1.Inv                                                                          Saving
* r1.Row                                     Imports                                                  Remitt
*
* r1.Expend


  TABLE p_SAM(r,rows,cols) "Social accounting matrix as total values (e.g. mio $), see SAM.xls"
*
*
*    Along one column: the "costs" or "expenditure"
*    Along one row:    the "revenues"
*
*
               S_Agr  S_Ind  S_SER   C_AGR  C_IND  C_SER    Lab     Cap      Hou       Gov       Inv     Revenue

  r1.S_Agr                             100                                               0                   100
  r1.S_Ind                                    200                                        0                   200
  r1.S_Ser                                           300                                 0                   300

  r1.C_Agr      30      20      10                                            40         0         0         100
  r1.C_Ind      30      30      30                                            90        10        10         200
  r1.C_Ser      10      30      40                                            90       120        10         300

  r1.Lab        10      10     200                                                                           220
  r1.Cap        10     100      10                                                                           120

  r1.Hou                                                    150      80                                      230
  r1.Gov        10      10      10                           70      40                                      130
  r1.Inv                                                                      10        10                    20

  r1.Expend    100     200     300     100    200    300    220     120      230       130       20

  ;
*
* --- check for completey empty SAM accounts
*
  set emptyRows(r,rows);
  emptyRows(r,rows) $ (not sum(cols,p_sam(r,rows,cols))) = YES;
  abort $ card(emptyRows) " Empty rows    in SAM for at least one region, in file: %system.fn%, line: %system.incline%",emptyRows,p_SAM;
  set emptyCols(r,cols);
  emptyCols(r,cols) $ (not sum(rows,p_sam(r,rows,cols))) = YES;
  abort $ card(emptyCols) " Empty columns in SAM for at least one region, in file: %system.fn%, line: %system.incline%",emptyCols,p_SAM;

  DISPLAY p_SAM;

  $$if not errorfree $abort Compilation errors related to SAM input, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*
*     (III) The CGE model itself
*
*
*---------------------------------------------------------------------------------------------------

  Parameter

     p_sigmap(r,a)     "Substitution elasticity between va and intermediate composite"
     p_sigmav(r,a)     "Substitution elasticity between factors"
     p_sigman(r,a)     "Substitution elasticity between intermediates"
     p_omegaf(r,f)     "Transformation elasticity in factor supply"

     p_io(r,c,a)       "Leontief input cofficients of intermediate demand"
     p_af(r,f,a)       "Share parameter for factor in va nest"
     p_and(r,a)        "Share parameter of intermediate composition in production outout"
     p_ava(r,a)        "Share parameter of value added in production outout"
     p_axp(r,a)        "Production frontier (shift parameter)"
     p_gf(r,f,a)       "Factor revenue shares in CET"

     p_oTax(r,c)       "Output taxes, charged on production"
     p_oTax0(r,c)      "Output taxes, charged on production, benchmark"
     p_fTax(r,f)       "Factor taxes"
     p_phi(r,dem)      "regional income shares"
     p_alphaa(r,c,aa)  "Demand value shares"

     p_xf(r,f)         "Total factor availabity"
     p_res(r,*,*,*,*)  "Result array"
  ;
  option p_res:2:3:1;

  $$if not errorfree $abort Compilation errors related to parameter declaration, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*
*     (III) The CGE model itself
*
*
*---------------------------------------------------------------------------------------------------

* (a) Declaration of variables

  positive Variables

    v_regy(r)           "Regional income"
    v_yTax(r,tax)       "Tax flows"
    v_yc(r)             "Household consumption expenditure"
    v_yg(r)             "Government consumption expenditure"
    v_ys(r)             "Saving consumption expenditure"

    v_px(r,c)           "Producer price"
    v_pva(r,s)          "Price of value added composite"
    v_pnd(r,s)          "Price of intermediate demand composite"
    v_pf(r,f)           "Factor prices"
    v_pfa(r,f,a)        "Sector specific factor prices"

    v_x(r,s)            "Output quantity"
    v_va(r,s)           "Value added quantity index"
    v_nd(r,s)           "Intermediate composite index"
    v_xf(r,f,s)         "Factor demand by sectors"
    v_xa(r,c,aa)        "Demand (Hou,gov,inv,intermediate by sectors)"

    V_sam(r,rows,cols)  "SAM"
  ;

* (b) Declaration of equations

  Equations
*
*   --- relating to SAM balancing
*
    e_expend(R,*)        "Definition of expenditure along one column of the SAM"
    e_Rev(R,*)           "Definition of revenue along one row of the SAM"
    e_Bal(R,*)           "Balance expenditure and revenue"
    e_MinDev              "Minimise normalised differences between original SAM and balanced one"
*
*   --- income generation distribution
*
    e_regy(r)           "Regional income"
    e_ytaxs(r,tax)      "Sectoral taxes"
    e_ytaxf(r,tax)      "Factor taxes"
    e_yc(r)             "Household consumption expenditure"
    e_yg(r)             "Government consumption expenditure"
    e_ys(r)             "Household consumption expenditure"
*
*   --- final demand
*
    e_xag(r,c,aa)       "Government demand, CD"
    e_xas(r,c,aa)       "Investment demand, CD"
    e_xah(r,c,aa)       "Household demand, LES"
*
*   --- production block
*
    e_va(r,a)           "Demand for value added"
    e_nd(r,a)           "Demand for intermediate composite"
    e_pva(r,a)          "Value added price"
    e_pnd(r,a)          "Intermediate composite price"
    e_px(r,c)           "Unit cost definition"
    e_xf(r,f,a)         "Factor demand"
    e_xaint(r,c,a)      "Intermediate demand"
    e_x(r,s)            "Market balance commodities"
    e_pf(r,f)          "CET Factor price aggregator"
    e_pfLIN(r,f)       "Market balance factor, linear aggregation"
    e_pfaLIN(r,f,a)    "Uniform factor prices, linear aggregation"
    e_pfaCET(r,f,a)    "Market balance factor, linear aggregation"
    e_xf(r,f,a)        "CET distribtion of factor supply to sector"


      e_dummy             "Dummy objective to use NLP instead CNS model"
  ;


  Free variables

      v_Dummy              "To construct a NLP"
      v_NrmErr             "Normalised deviation between balanced and given SAM"
      v_Gamma              "Minimum commitment levels of LES demand system"
      ;

  $$if not errorfree $abort Compilation errors related to parameter declaration, in file: %system.fn% , before line: %system.incline%

*
* (e) Definition of equations
*

*
* -------------------------------------------------------------------------------------------------------------------------
*
*      SAM balancing
*
* -------------------------------------------------------------------------------------------------------------------------
*

*
* --- define expenditure for each column as sum over the rows
*
  e_expend(R,BCols)  ..   v_sam(R,"Expend",BCols)    =E= SUM(BRows,  v_sam(R,BRows,BCols));
*
* --- define revenues for each row as sum over the columns
*
  e_Rev(R,BRows)  ..      v_sam(R,BRows,"Revenue")   =E= SUM(BCols,  v_sam(R,BRows,BCols));
*
* --- revenues and expenditure for matching rows/columns must be equal
*
  e_Bal(R,BRows)  ..      v_sam(R,BRows,"Revenue")   =E= sum(sameas(BRows,Bcols),v_sam(R,"Expend",BCols));

*
* --- define average squared relative differnces against given SAM entries
*
  e_MinDev     ..

     v_NrmErr * sum( (R,Rows,Cols) $ p_SAM(R,Rows,Cols), 1.)/100.
            =E=
                sum( (R,Rows,Cols) $ p_SAM(R,Rows,Cols),
                                    SQR( (v_sam(R,Rows,Cols)-p_SAM(R,Rows,Cols))/p_SAM(R,Rows,Cols)));

*
* -------------------------------------------------------------------------------------------------------------------------
*
*      income generation and distribution
*
* -------------------------------------------------------------------------------------------------------------------------
*

*
* --- regional income: tax income and factor income
*
  e_regy(r) ..

    v_regy(r) =E= sum(tax, v_yTax(r,tax)) + sum( f, p_xf(r,f) * v_pf(r,f)*(1-p_fTax(r,f)));
*
* --- taxes on output
*
  e_ytaxS(R,"s") ..

    v_yTax(R,"s")  =E= SUM(s_to_c(s,c), v_x(r,s) * v_px(r,c) * p_oTax(r,c));
*
* --- taxes on factor income
*
  e_ytaxF(R,"f") ..

    v_yTax(R,"f")  =E= SUM( f,    p_xf(r,f) * v_pf(r,f) * p_fTax(r,f));
*
* --- household consumption expenditure, value share on total regional income
*
  e_yc(r) ..

    v_yc(r) =E= v_regy(r) * p_phi(r,"hou");
*
* --- government consumption expenditure, value share on total regional income
*
  e_yg(r) ..

    v_yg(r) =E= v_regy(r) * p_phi(r,"gov");
*
* --- savings consumption expenditure, value share on total regional income
*
  e_ys(r) ..

    v_ys(r) =E= v_regy(r) * p_phi(r,"inv");

* -------------------------------------------------------------------------------------------------------------------------
*
*     final demand
*
* -------------------------------------------------------------------------------------------------------------------------

*
* --- government demand is CD = fixed value shares
*
  e_xag(r,c,"gov") ..

    v_xa(r,c,"gov") * v_px(r,c)*(1+p_oTax(r,c)) =E= p_alphaa(r,c,"gov") * v_yg(r);
*
* --- investmend demand is CD = fixed value shares
*
  e_xas(r,c,"inv") ..

    v_xa(r,c,"inv") * v_px(r,c)*(1+p_oTax(r,c)) =E= p_alphaa(r,c,"inv") * v_ys(r);

*
* --- household demand, LES
*
  e_xah(r,c,"hou") ..

    v_xa(r,c,"hou") =E=
                        v_Gamma(r,c) + p_alphaa(R,c,"hou")/[v_px(r,c)*(1+p_oTax(r,c))]
                          * (v_yc(r) -SUM(c1, v_Gamma(r,c1)*v_px(R,c1)*(1+p_oTax(r,c1))   ));

* -------------------------------------------------------------------------------------------------------------------------
*
*     production block
*
* -------------------------------------------------------------------------------------------------------------------------

* --- Unit cost definition (output tax inclusive)
*     Output price (= marginal revenue) = marginal cost = dual price aggregator of top level CES

  e_px(r,c) ..
*
     sum(s_to_c(a,c),p_axp(r,a))*v_px(r,c) =E=

     sum(s_to_c(a,c),(   p_and(r,a)*v_pnd(r,a)**(1-p_sigmap(r,a))
                       + p_ava(r,a)*v_pva(r,a)**(1-p_sigmap(r,a))) ** (1/(1-p_sigmap(r,a))));

* --- Value added price: dual price aggreagator

  e_pva(r,a) ..
*
     v_pva(r,a) =E= sum(f, p_af(r,f,a) * v_pfa(r,f,a)**(1-p_sigmav(r,a)))** ( 1/(1-p_sigmav(r,a)));

* --- Intermediate composite price: dual price aggreagator

  e_pnd(R,s) ..
*
     v_pnd(r,s) =E= sum(c, p_io(r,c,s) *  [v_px(r,c)*(1+p_oTax(r,c))]**(1-p_sigman(r,s)))** ( 1/(1-p_sigman(r,s)));

* --- Demand for intermediate composite

  e_nd(R,s) ..
*
     v_nd(r,s) =E=  p_and(r,s)* v_x(r,s) * (sum(s_to_c(s,c),v_px(r,c)) / v_pnd(r,s)) ** p_sigmap(r,s)
                      * p_axp(r,s) ** (p_sigmap(r,s)-1);

* --- Demand for value added aggregate

  e_va(R,s) ..
*
     v_va(r,s) =E=  p_ava(r,s)* v_x(r,s) * (sum(s_to_c(s,c),v_px(r,c))/ v_pva(r,s)) ** p_sigmap(r,s)
                      * p_axp(r,s) ** (p_sigmap(r,s)-1);

* --- Factor demand

  e_xf(r,f,a) ..

     v_xf(r,f,a) =E= p_af(r,f,a) * v_va(r,a) * (v_pva(r,a)/v_pfa(r,f,a))**p_sigmav(r,a);

* --- Intermediate demand

  e_xaint(r,c,a) ..

     v_xa(r,c,a) =E= p_io(r,c,a) * v_nd(r,a) * (v_px(r,c)*(1+p_oTax(r,c))/v_pnd(r,a))**p_sigman(r,a);

* -------------------------------------------------------------------------------------------------------------------------
*
*     market clearing
*
* -------------------------------------------------------------------------------------------------------------------------

*
* --- total output must be exhausted by intermediate and final demand
*
  e_x(r,s) ..

     v_x(r,s) =E= sum((s_to_c(s,c),aa), v_xa(r,c,aa));

*
* --- Dual price aggregator for factor price based on CET
*
  e_pf(r,f) $ (p_omegaf(r,f) ne inf) ..

     v_pf(r,f)  =e= sum(a, p_gf(r,f,a)*v_pfa(r,f,a)**(1+p_omegaf(r,f)))**(1/(1+p_omegaf(r,f)));
*
* --- Market claring for factor in case of linear aggregation
*     (infinite transformation elasticity), implicitly defines factor prices
*
  e_pfLin(r,f) $ (p_omegaf(r,f) eq inf) ..

     p_xf(r,f)  =e= sum(s, v_xf(r,f,s));
*
* --- supply to sectors based on CET, define sector specific factor price
*
  e_pfaCET(r,f,a) $ (p_omegaf(r,f) ne inf) ..

     v_xf(r,f,a) =E= p_gf(r,f,a) * p_xf(r,f) * (v_pfa(r,f,a)/v_pf(r,f))**p_omegaf(r,f);

*
* -- sector specific factor prices are all identical in case of linear
*    aggregation
*
  e_pfaLin(r,f,a) $ (p_omegaf(r,f) eq inf) ..

     v_pfa(r,f,a) =E= v_pf(r,f);

*
* ---------------------------------------------------------------------------------------------------------------------
*
  e_Dummy      ..    v_dummy =E= 10;

  $$if not errorfree $abort Compilation errors in equation definitions, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*
*     (IV) Data preparation: balance the SAM
*
*
*---------------------------------------------------------------------------------------------------

*
* --- the following model comprises the balances in value terms, mostly
*     and minimizes deviation between the given SAM and the balanced one
*     stored in the "BAL" column after the calibration step
*

  MODEL m_balSam /
        e_expend
        e_Rev
        e_Bal
        e_MinDev
  /;

  m_balSam.Solprint  = 1;
  m_balSam.Limcol    = 0;
  m_balSam.Limrow    = 0;
  m_balSam.Holdfixed = 1;
  m_balSam.optfile   = 1;

* --- calculate the expenditure/revenue

  p_sam(R,"expend",BCols ) = SUM( BRows,  p_sam(R,BRows,BCols));
  p_sam(R,BRows,"revenue") = SUM( BCols,  p_sam(R,BRows,BCols));

* --- Store the unbalanced SAM in the result array

  p_res(R,Rows,Cols,"V","SAM") = p_SAM(R,Rows,Cols);
*
* --- Initialise variables to the values of the unbalanced SAM
*     and introduce lower and upper limits

  v_sam.L(R,Rows,Cols)  = p_SAM(R,Rows,Cols);
  v_sam.LO(R,Rows,Cols) = p_SAM(R,Rows,Cols) * 0.1;
  v_sam.UP(R,Rows,Cols) = p_SAM(R,Rows,Cols) * 10.;

* --- Balance the SAM

  SOLVE m_balSam MINIMIZING v_NrmErr USING NLP;
  IF ( m_balSam.numInfes > 0,
     Abort "SAM could not be balanced, Stop, in file: %system.fn% , before line: %system.incline%";
  );

  abort $ sum((r,rows,cols),v_sam.l(r,rows,cols) lt 0)
    " Negative SAM entries after balancing step, in file: %system.fn%, line: %system.incline%",v_sam.l;
*
* --- independent check if balancing model worked
*
  parameter p_samCheck(r,*,*);
  $$batinclude 'samCheck.gms' 'SAM balancing'

*---------------------------------------------------------------------------------------------------
*
*     (Va) Calibration of factor supply
*
*---------------------------------------------------------------------------------------------------

*
* --- Full labor mobility
*
  p_omegaf(r,"lab") = inf;
*
* --- Sluggish capital mobiluity
*
  p_omegaf(r,"cap") = 0.5;
*
* --- check for negative transformation elasticities
*
  abort $ sum((r,f),(p_omegaf(r,f) lt 0))
   " Check for non-negative transformation elasticities failed, in file: %system.fn%, line: %system.incline%",p_omegaf;
*
* --- factor prices are unity
*
  v_pf.l(r,f)     = 1;
  v_pfa.l(r,f,a)  = 1;
*
* --- assign factor use per sector and in total
*
  v_xf.fx(r,f,s)  = v_sam.l(R,f,s);
*
* --- total factor use
*
  p_xf(r,f)      = sum(s, v_xf.l(r,f,s));
*
* --- factor use shares
*
  p_gf(r,f,a)    = v_xf.l(r,f,a)/p_xf(r,f);

  v_pf.fx(r,f) $ (p_omegaf(r,f) eq inf) = 1;

  model m_fac /
                e_pf.v_pf
                e_pfLIN.v_pf
                e_pfaLIN.v_pfa
                e_pfaCET.v_pfa
  /;
  m_fac.iterlim   = 0;
  m_fac.holdfixed = 1;
*
* --- test calibation at fixed prices
*
  solve m_fac using MCP;
  abort $ m_fac.sumInfes " Benchmarking of factor supply failed, in file: %system.fn%, line: %system.incline%",emptyCols,p_SAM;

  $$if not errorfree $abort Compilation errors benchmarking of factor supply, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*     (Vb) Calibration of production side
*
*---------------------------------------------------------------------------------------------------

*
* --- subsitution elasticities, we mimick the GTAP Standard model
*
*     (1) No substitution between VA and intermediate composite (= LEONTIEF)
*     (3) No substitution inside intermediate composite (=LEONTIEF)
*     (4) Substituion between factors inside va-nest
*
  p_sigmap(r,s) = 0;
  p_sigman(r,s) = 0;
  p_sigmav(r,s) = 0.5;

  abort $ sum((r,s),(p_sigmap(r,s) lt 0))
   " Check for non-negative subsitution elasticities between VA and ND failed, in file: %system.fn%, line: %system.incline%",p_sigmap
  abort $ sum((r,s),(p_sigman(r,s) lt 0))
   " Check for non-negative subsitution elasticities between intermediates failed, in file: %system.fn%, line: %system.incline%",p_sigman;
  abort $ sum((r,s),(p_sigmav(r,s) le 0))
   " Check for positive substitution elasticities in value-added nest failed, in file: %system.fn%, line: %system.incline%",p_sigmav;
*
* --- prices are unity in benchmark
*     (= SAM values are interpreted as quantity indices)
*
  v_pnd.l(r,s)    = 1;
  v_pva.l(r,s)    = 1;
  v_pfa.fx(r,f,a) = 1;


  v_xf.lo(r,f,s)  = 0;
  v_xf.up(r,f,s)  = inf;
*
* --- calculate value added
*
  v_va.l(r,s)  = sum(f,v_xf.l(r,f,s));
*
* --- assign intermediate demand for each commodity
*     and composite
*
  v_xa.l(r,c,s)  = v_sam.l(R,c,s);
  v_nd.l(r,s)    = sum(c,v_xa.l(r,c,s));
*
* --- output tax rate calculate from SAM
*
  p_oTax(r,c)     = sum(s_to_c(s,c),  v_sam.l(r,"gov",s)/(v_sam.l(r,"expend",s)-v_sam.l(r,"gov",s)));
*
* --- check for unreasonable large negative output tax rates
*
  abort $ sum((r,c), (p_oTax(r,c) lt -0.9))
   " Unreasonably high negative output tax rate found, in file: %system.fn%, line: %system.incline%",p_oTax;

  p_oTax0(r,c)    = p_oTax(r,c);

  v_px.l(r,c) = 1/(1+p_oTax(r,c));
*
* --- total production
*
  v_x.fx(r,s)    = (v_va.l(r,s) + v_nd.l(r,s))*sum(s_to_c(s,c),1+p_oTax(r,c));
*
* --- define share parameters
*
  p_ava(r,s)     = [v_va.l(r,s)/v_x.l(r,s)] / (sum(s_to_c(s,c),v_px.l(r,c)) / v_pva.l(r,s)) ** p_sigmap(r,s) ;
  abort $ sum((r,s), (p_ava(r,s) lt 0))
   " Negative cost share parameters for value added nest found, in file: %system.fn%, line: %system.incline%",p_ava;

  p_and(r,s)     = [v_nd.l(r,s)/v_x.l(r,s)] / (sum(s_to_c(s,c),v_px.l(r,c)) / v_pnd.l(r,s)) ** p_sigmap(r,s) ;
  abort $ sum((r,s), (p_and(r,s) lt 0))
   " Negative cost share parameters for ND nest found, in file: %system.fn%, line: %system.incline%",p_ava;

  p_axp(r,s)     = 1;

  p_io(r,c,s)    = v_xa.l(r,c,s)/v_nd.l(r,s);
  abort $ sum((r,c,s), (p_io(r,c,s) lt 0))
   " Negative IO parameters found, in file: %system.fn%, line: %system.incline%",p_io;

  p_af(r,f,s)    = v_xf.l(r,f,s)/v_va.l(r,s);
  abort $ sum((r,c,s), (p_io(r,c,s) lt 0))
   " Negative factor cost share parameters found, in file: %system.fn%, line: %system.incline%",p_af;


  model m_sup /
                e_va.v_va,
                e_nd.v_nd,
                e_pva.v_pva,
                e_pnd.v_pnd,
                e_xf.v_xf,
                e_xaint.v_xa,
                e_px.v_px
             /;
  m_sup.iterlim   = 0;
  m_sup.holdfixed = 1;
*
* --- test calibation at fixed input prices and given output quantitie
*
  m_sup.iterlim=0;
  solve m_sup using MCP;
  abort $ m_sup.sumInfes " Benchmarking of production functions failed, in file: %system.fn%, line: %system.incline%",emptyCols,p_SAM;

  $$if not errorfree $abort Compilation errors in benchmarking of production functions, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*     (Vc) Calibration of income generation and income distribution
*
*---------------------------------------------------------------------------------------------------
*
* --- factor taxes
*
  p_fTax(r,f)     = sum(sameas(cols,f),v_sam.l(r,"gov",cols)/v_sam.l(r,"expend",cols));
*
* --- Output tax and factor tax revenues
*
  v_yTax.l(r,"s")  = sum(s_to_c(s,c), v_x.l(r,s) * v_px.l(r,c) * p_oTax(r,c));
  v_yTax.l(r,"f")  = sum(f,           p_xf(r,f)  * v_pf.l(r,f) * p_fTax(r,f));
*
* --- regional income: total tax income plus factor income net of taxes
*
  v_regy.l(r) =   sum( tax,   v_yTax.l(r,tax))
                + sum( (f,s), v_xf.l(r,f,s) * (v_pf.l(r,f)*(1-p_fTax(r,f))));
*
* --- income distribution
*
  v_yc.l(r) = v_sam.l(r,"expend","hou") - v_sam.l(r,"inv","hou");
  abort $ sum(r, (v_yc.l(r) le 0))
   " Zero or negative expenditures for private demand found, in file: %system.fn%, line: %system.incline%",v_yc.l;

  v_yg.l(r) = v_sam.l(r,"expend","gov") - v_sam.l(r,"inv","gov");
  abort $ sum(r, (v_yg.l(r) le 0))
   " Zero or negative expenditures for private demand found, in file: %system.fn%, line: %system.incline%",v_yg.l;

  v_ys.l(r) = v_sam.l(r,"expend","inv");
  abort $ sum(r, (v_ys.l(r) le 0))
   " Zero or negative investments expenditures found, in file: %system.fn%, line: %system.incline%",v_ys.l;

  p_phi(r,"hou") = v_yc.l(r) / v_regy.l(r);
  p_phi(r,"gov") = v_yg.l(r) / v_regy.l(r);
  p_phi(r,"inv") = v_ys.l(r) / v_regy.l(r);

  parameter p_testPhi; p_testPhi(r) = sum(dem, p_phi(r,dem));
  abort $ sum(r $ (p_testPhi(r) ne 1),1) "Inconsistent income distribution shares, in file: %system.fn%, line: %system.incline%", p_testPhi;
*
* --- factor use and output is fixed in the following sub-model
*
  v_xf.fx(r,f,s) = v_xf.l(r,f,s);
  v_x.fx(r,s)    = v_x.l(r,s);

  v_pf.fx(r,f)   = v_pf.l(r,f);
  v_px.fx(r,c)   = v_px.l(r,c);


  model m_inc /
                e_regy.v_regy
                e_yTaxS.v_yTax
                e_yTaxF.v_yTax
                e_yc.v_yc
                e_yg.v_yg
                e_ys.v_ys
              /;

  m_inc.iterlim   = 0;
  m_inc.holdfixed = 1;
  solve m_inc using MCP;
  abort $ m_sup.sumInfes " Benchmarking of income distribution failed, in file: %system.fn%, line: %system.incline%",emptyCols,p_SAM;

  $$if not errorfree $abort Compilation errors in benchmarking of income distribution, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*     (Vd) Calibration of final demand
*
*---------------------------------------------------------------------------------------------------

*
* --- assign government demand and calculate value shares
*
  v_xa.l(r,c,"gov")   = v_sam.l(r,c,"gov");
  p_alphaa(r,c,"gov") = v_xa.l(r,c,"gov") / v_yg.l(r);
  abort $ sum((r,c),(p_alphaa(r,c,"gov") lt 0))
   " Negative budget shares for public demand found, in file: %system.fn%, line: %system.incline%",p_alphaa;

  abort $ sum(r, sum(c,p_alphaa(r,c,"gov")) ne 1)
   " Budget shares for public demand don't add up to unity, in file: %system.fn%, line: %system.incline%",p_alphaa;
*
* --- assign investment demand and calculate value shares
*
  v_xa.l(r,c,"inv")   = v_sam.l(r,c,"inv");
  p_alphaa(r,c,"inv") = v_xa.l(r,c,"inv") / v_ys.l(r);
  abort $ sum((r,c),(p_alphaa(r,c,"inv") lt 0))
   " Negative budget shares for investment demand found, in file: %system.fn%, line: %system.incline%",p_alphaa;

  abort $ sum(r, sum(c,p_alphaa(r,c,"inv")) ne 1)
   " Budget shares for investment demand don't add up to unity, in file: %system.fn%, line: %system.incline%",p_alphaa;

* ---- set marginal expenditure shares for LES demand system

  v_xa.fx(r,c,"hou") = V_sam.l(r,c,"hou");

  p_alphaa(R,"C_Agr","hou") = 0.1;
  p_alphaa(R,"C_Ind","hou") = 0.4;
  p_alphaa(R,"C_Ser","hou") = 0.5;

  abort $ sum((r,c),(p_alphaa(r,c,"hou") lt 0))
   " Negative budget shares for private demand found, in file: %system.fn%, line: %system.incline%",p_alphaa;

  abort $ sum(r, sum(c,p_alphaa(r,c,"hou")) ne 1)
   " Budget shares for private demand don't add up to unity, in file: %system.fn%, line: %system.incline%",p_alphaa;

  v_gamma.LO(R,c)     = v_sam.l(R,c,"Hou") * 0.0;
  v_gamma.UP(R,c)     = v_sam.l(R,c,"Hou") * 0.9;
  v_gamma.UP(R,c)     = inf;
  v_gamma.l(R,c)      = 0;
*
* ---- Calibration model for CES share and shift parameters,
*      minimum commitment levels Gamma for LES demand system
*      and calculation of Utility

  Model m_calLES /e_xah ,e_dummy/;
  m_calLES.Solprint  = 1;
  m_calLES.Limcol    = 0;
  m_calLES.Limrow    = 0;
  m_calLES.Holdfixed = 0;
  m_calLES.optfile   = 1;

* ---- Fix variables to balanced SAM

  v_yc.fx(r)         = V_yc.l(r);

  solve m_calLES minimizing v_dummy using nlp;
  abort $ m_calLes.sumInfes " Benchmarking of LES demand system failed, in file: %system.fn%, line: %system.incline%",emptyCols,p_SAM;

* --- fix parameters to calibrated solution and de-fix demand

  v_Gamma.FX(R,c)      = v_Gamma.L(R,c);

  v_xa.lo(r,c,"Hou") = 0;
  v_xa.up(r,c,"Hou") = inf;

  v_yg.fx(r) = v_yg.l(r);
  v_ys.fx(r) = v_ys.l(r);
  v_yc.fx(r) = v_yc.l(r);


  model m_dem / e_xag.v_xa
                e_xas.v_xa
                e_xah.v_xa
  /;
  m_dem.iterlim   = 0;
  m_dem.holdfixed = 1;
  solve m_dem using MCP;
  abort $ m_dem.sumInfes " Benchmarking of LES demand system failed, in file: %system.fn%, line: %system.incline%",emptyCols,p_SAM;

  $$if not errorfree $abort Compilation errors in benchmarking of demand systems, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*     (Ve) define full model
*
*---------------------------------------------------------------------------------------------------

  model m_cge / m_sup
                m_inc
                m_dem
                m_fac
*               --- market balances
                e_x.v_x
  /;

  m_cge.holdfixed = 1;
  m_cge.limrow = 0;
  m_cge.limcol = 0;
  m_cge.solvelink = 5;
*
* --- remove fixing of variable for test solves / calibration above
*
  v_yc.lo(r) = 0;
  v_yc.up(r) = inf;

  v_yg.lo(r) = 0;
  v_yg.up(r) = inf;

  v_ys.lo(r) = 0;
  v_ys.up(r) = inf;

  v_xa.lo(r,c,"hou") = 0;
  v_xa.up(r,c,"hou") = inf;

  v_pf.lo(r,f) = 0;
  v_pf.up(r,f) = inf;

  v_pfa.lo(r,f,a) = 0;
  v_pfa.up(r,f,a) = inf;

  v_x.lo(r,s)  = 0;
  v_x.up(r,s)  = inf;

  v_xf.lo(r,f,s)  = 0;
  v_xf.up(r,f,s)  = inf;

  v_px.lo(r,c)  = 0;
  v_px.up(r,c)  = inf;

  v_pnd.lo(r,s)   = 0;
  v_pnd.up(r,s)   = inf;

  v_pva.lo(r,s)   = 0;
  v_pva.up(r,s)   = inf;

*---------------------------------------------------------------------------------------------------
*
*     (VI) Check for calibration and homogenity of degree one / zero in prices
*
*---------------------------------------------------------------------------------------------------

   set checkScens / Bench,HomogT,Test /;
*
* --- Benchmark test: Are all model equations feasible at start point? (set solver iterations to zero)
*
  m_cge.iterlim = 0;
  solve m_cge using MCP;
  abort $ m_cge.sumInfes " Benchmarking test not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'Bench'"
*
* --- Homogeniety test: change numeraire price by 10% and check of product and factor demands are unchanged
*
  v_px.fx(r,c) $ (c.pos eq 1) = v_px.l(r,c)*1.1;
  m_cge.iterlim = 10000;
  solve m_cge using MCP;
  abort $ m_cge.sumInfes " Homogeniety test for full model not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'HomogT'"

  abort $ sum((r,c,aa),abs(p_res(r,c,aa,"q","Bench") - p_res(r,c,aa,"q","HomogT")) gt 1.E-8)
   " Homogeniety test failed for product demands, check r-c-aa-q in p_res,  in file: %system.fn%, line: %system.incline%",p_res;

  abort $ sum((r,f,a),abs(p_res(r,f,a,"q","Bench") - p_res(r,f,a,"q","HomogT")) gt 1.E-8)
   " Homogeniety test failed for factor demands, check r-f-a in p_res, in file: %system.fn%, line: %system.incline%",p_res;
*
* --- Simulation check: reset numeraire to unity, run simulation, and check that product and factor demand are unchanged
*
  v_px.fx(r,c) $ (c.pos eq 1) = v_px.l(r,c)/1.1;
  m_cge.iterlim = 10000;
  solve m_cge using MCP;
  abort $ m_cge.sumInfes " Test with benchmark price solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'Test'"

  abort $ sum((r,c,aa),abs(p_res(r,c,aa,"q","Bench") - p_res(r,c,aa,"q","Test")) gt 1.E-8)
   " Simulation test at unchanged prices failed for product demands, check r-c-aa-q in p_res,  in file: %system.fn%, line: %system.incline%",p_res;

  abort $ sum((r,f,a),abs(p_res(r,f,a,"q","Bench") - p_res(r,f,a,"q","Test")) gt 1.E-8)
   " Simulation test at unchanged prices failed for factor demands, check r-f-a in p_res, in file: %system.fn%, line: %system.incline%",p_res;

  $$if not errorfree $abort Compilation errors in test runs, in file: %system.fn% , before line: %system.incline%

*---------------------------------------------------------------------------------------------------
*
*     (VIII) Simulations
*
*---------------------------------------------------------------------------------------------------


* --- (a) increase capital endowment of households by 10% => higher capital availability in the economy

  p_xf(R,"Cap") = p_xf(r,"cap") * 1.10;

  SOLVE m_cge using MCP;
  abort $ m_cge.sumInfes " Simulation with increased capital supply not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'More cap'"

  p_xf(R,"Cap") = p_xf(r,"cap") / 1.10;

* --- (b) increase labor supply of households by 10% => higher labor availability in the economy

  p_xf(R,"lab") = p_xf(r,"lab") * 1.10;

  SOLVE m_cge using MCP;
  abort $ m_cge.sumInfes " Simulation with increased labor supply not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'More lab'"
  p_xf(R,"lab") = p_xf(r,"lab") / 1.10;

* --- (c) %1 t.p. in all sector

  p_axp(r,s) = 1.01;
  SOLVE m_cge using MCP;
  abort $ m_cge.sumInfes " Simulation with high technial progress not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'1%Tp'"
  p_axp(r,s) = 1;

* --- (d) half output taxes

  p_oTax(r,c) = p_oTax(r,c) * 0.5;

  SOLVE m_cge using MCP;
  abort $ m_cge.sumInfes " Simulation with halved output taxes not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'1%Tp'"
  p_oTax(r,c) = p_otax(r,c) * 2.0;

* --- (e) remove factor taxes

  p_fTax(r,f) = eps;
  SOLVE m_cge using MCP;
  abort $ m_cge.sumInfes " Simulation with removed factor taxes not solved without infeasibilities, in file: %system.fn%, line: %system.incline%";
  $$batinclude "store_res.gms" "'no_fTax'"

  display p_res;

  $$if not errorfree $abort Compilation errors in simulation runs, in file: %system.fn% , before line: %system.incline%
