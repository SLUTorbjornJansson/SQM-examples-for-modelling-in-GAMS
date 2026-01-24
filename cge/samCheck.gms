********************************************************************************
$ontext

   Simulation model for policy analysis

   GAMS file : SAMCHECK.GMS

   @purpose  : Check if a given SAM stored on variable VSAM.L is balanced
   @author   : W.Britz
   @date     : 23.01.26
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : cge.gms, store_res.gms

$offtext
********************************************************************************
  option kill=p_samCheck;
*
* --- define sum over columns/rows and define difference
*
  p_samCheck(r,"expend",bCols)   = sum(bRows,v_SAM.l(r,bRows,bCols));
  p_samCheck(R,bRows,"revenue")  = sum(bCols,v_SAM.l(R,BRows,BCols));
  p_samCheck(R,"diff",BCols)     = p_samCheck(r,"expend",bCols) - sum(sameas(bCols,bRows),p_samCheck(r,bRows,"revenue"));

* --- remove differences < 1.E-6 (solver feasibility)
  p_samCheck(R,"diff",BCols)   $ (abs(p_samCheck(R,"diff",BCols)) le 1.E-6) = 0;
*
* --- delete sums without errors
*
  p_samCheck(r,"expend",BCols)  $ (not p_samCheck(R,"diff",BCols)) = 0;
  p_samCheck(r,bRows,"revenue") $ (not sum(sameas(bRows,bCols), p_samCheck(R,"diff",BCols))) = 0;

  abort $ card(p_samCheck) " Check for balanced SAM failed for '%1', in file: %system.fn%, line: %system.incline%",p_samCheck,v_sam.l;
  if ( execerror, abort "Run-Time error in file: %system.fn%, line: %system.incline%");
  $$if not errorfree $abort Compilation errors in SAM balancing, in file: %system.fn% , before line: %system.incline%
