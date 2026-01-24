********************************************************************************
$ontext

   Didactic multi-market model

   GAMS file : STORE_RES.GMS

   @purpose  : Store and check results
   @author   : W.Britz
   @date     : 24.01.26
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : Multi-market.gms

$offtext
********************************************************************************
*
* --- non-negativity checks
*
  abort $ sum(i, v_sup.l(i) lt 0)
     "Negative supply quantities found in simulation %1, in file: %system.fn%, line: %system.incline%",v_sup.l;

  abort $ sum(i, v_dem.l(i) lt 0)
     "Negative demand quantities found in simulation %1, in file: %system.fn%, line: %system.incline%",v_dem.l;

  abort $ sum(i, v_p.l(i) lt 0)
     "Negative demand quantities found in simulation %1, in file: %system.fn%, line: %system.incline%",v_p.l;
*
* --- assign results
*
  p_res(i,"supply",%1) = v_sup.l(i);
  p_res(i,"demand",%1) = v_dem.l(i);
  p_res(i,"p_sup",%1)  = v_p.l(i) + p_subs(i);
  p_res(i,"p_dem",%1)  = v_p.l(i);
*
* --- check for closed markets
*
  abort $ sum(i, abs(v_sup.l(i)-v_dem.l(i)) gt 1.E-6)
     "Markets not closed in simulation %1, in file: %system.fn%, line: %system.incline%",v_sup.l,v_dem.l;

 if ( execerror, abort "Run-Time error in file: %system.fn%, line: %system.incline%");
 $$if not errorfree $abort Compilation error after file: %system.fn%
