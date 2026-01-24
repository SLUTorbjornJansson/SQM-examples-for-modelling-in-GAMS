********************************************************************************
$ontext

   Modeling of Agri-Ecological Systems

   GAMS file : STORE_RES.GMS

   @purpose  : Store results from farm model to array. Some result checks
               Argument: %1 name of scenario to store
                         %2 time points to store
                         %3 time points to calculate averages
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

  p_problem2D(herds,%2) $ (v_herd.l(herds,%2) lt 0) = v_herd.l(herds,%2);
  $$batinclude "../shared/assert_no_problem.gms" p_problem2D "Negative herd size found for %1, in file: %system.fn%, line: %system.incline%"

  p_problem3D(herds,feeds,%2) $ (v_feeding.l(herds,feeds,%2) lt 0) = v_feeding.l(herds,feeds,%2);
  $$batinclude "../shared/assert_no_problem.gms" p_problem3D "Negative feed quantities found for %1, in file: %system.fn%, line: %system.incline%"
*
* --- assign variables to result array
*
  p_res(%1,herds,%2)        = v_herd.l(herds,%2);
  p_res(%1,feeds,%2)        = sum(herds,v_feeding.l(herds,feeds,%2));
  p_res(%1,"sellCows",%2)   = v_sellCows.l(%2);
  p_res(%1,"buyCows",%2)    = v_buyCows.l(%2);
*
* --- add eps to total farm results to indicate solutions where the farm was not realized
*
  p_res(%1,"gm",%2)         = v_gm.l(%2)  + eps;
  p_res(%1,"ghg",%2)        = v_ghg.l(%2) + eps;
  p_res(%1,"obje",%2)       = v_obje.l    + eps;
*
* --- calculate averages over time
*     (Note: this will give the right results in case of rec-dyn for the call of the
*            last time point)
*
  p_res(%1,"ghg","avg")     = sum(%3,p_res(%1,"ghg",%3))/card(%3);
  p_res(%1,"gm","avg")      = sum(%3,p_res(%1,"gm",%3))/card(%3);
  p_res(%1,herds,"avg")     = sum(%3,p_res(%1,herds,%3))/card(%3);

  if ( execerror, abort "Run-Time error in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation error after file: %system.fn%
