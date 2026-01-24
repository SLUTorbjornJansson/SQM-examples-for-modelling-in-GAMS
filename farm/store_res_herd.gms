********************************************************************************
$ontext

   Modeling of Agri-Ecological Systems

   GAMS file : STORE_RES.GMS

   @purpose  : Store results from farm model to array. Some result checks
   @author   :
   @date     : 05.04.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : herd_dyn.gms

$offtext
********************************************************************************
*
* --- checks for non-negativity of selected results
*
  p_problem1D("") $ (v_obje.l lt 0) = v_obje.l;
  $$batinclude "../shared/assert_no_problem.gms" p_problem1D "Negative objective value found for %1, in file: %system.fn%, line: %system.incline%"

  p_problem2D(herds,t1) $ (v_herd.l(herds,t1) lt 0) = v_herd.l(herds,t1);
  $$batinclude "../shared/assert_no_problem.gms" p_problem2D "Negative herd size found for %1, in file: %system.fn%, line: %system.incline%"

  p_problem3D(herds,feeds,t1) $ (v_feeding.l(herds,feeds,t1) lt 0) = v_feeding.l(herds,feeds,t1);
  $$batinclude "../shared/assert_no_problem.gms" p_problem3D "Negative feed quantities found for %1, in file: %system.fn%, line: %system.incline%"
*
* --- assign variables to result array
*
  p_res(%1,herds,t1)        = v_herd.l(herds,t1);
  p_res(%1,feeds,t1)        = sum(herds,v_feeding.l(herds,feeds,t1));
  p_res(%1,"sellCows",t1)   = v_sellCows.l(t1);
  p_res(%1,"sellCows+1",t1) = v_sellCows.l(t1+1);
  p_res(%1,"buyCows",t1)    = v_buyCows.l(t1);
  p_res(%1,"gm",t1)         = v_gm.l(t1)  + eps;
  p_res(%1,"ghg",t1)        = v_ghg.l(t1) + eps;
  p_res(%1,"obje",t1)       = v_obje.l + eps;
  p_res(%1,"gm+1",t1)       = v_gm.l(t1+1) + eps;
*
* --- calculate averages over time
*
  p_res(%1,"ghg","avg")     = sum(t,p_res(%1,"ghg",t))/card(t);
  p_res(%1,"gm","avg")      = sum(t,p_res(%1,"gm",t))/card(t);
  p_res(%1,herds,"avg")     = sum(t,p_res(%1,herds,t))/card(t);

  if ( execerror, abort "Run-Time error in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation error after file: %system.fn%
