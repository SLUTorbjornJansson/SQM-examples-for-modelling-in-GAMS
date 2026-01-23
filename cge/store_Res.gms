********************************************************************************
$ontext

   Simulation model for policy analysis

   GAMS file : STORE_RES.GMS

   @purpose  :
   @author   :
   @date     : 22.03.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************
*
*  --- regional income
*
   p_res(r,"regy","v","v",%1) = v_regy.l(r);
*
*  --- utility of the institutions
*
   p_res(r,"gov","u","v",%1)
       = prod(c $ p_alphaa(r,c,"gov"),
             v_xa.l(r,c,"gov")**p_alphaa(r,c,"gov") );

   p_res(r,"inv","u","v",%1)
       = prod(c $ p_alphaa(r,c,"inv"),
             v_xa.l(r,c,"inv")**p_alphaa(r,c,"inv") );

   p_res(r,"hou","u","v",%1)
       = prod(c $ p_alphaa(r,c,"hou"),
             (v_xa.l(r,c,"hou")-v_gamma.l(r,c))**p_alphaa(r,c,"hou") );

   p_res(r,"hou","u1","v",%1)
       = (v_yc.l(r) - sum(c, v_gamma.l(r,c)*v_px.l(r,c)*(1+p_oTax(r,c))))

           * prod(c $ p_alphaa(r,c,"hou"),
                (p_alphaa(r,c,"hou")/[v_px.l(r,c)*(1+p_oTax(r,c))])**p_alphaa(r,c,"hou") );
*
*  --- price indices = spent divided by utility
*
   p_res(r,c,aa,"p",%1) = v_px.l(r,c) * (1+p_oTax(r,c));

   p_res(r,"gov","p","v",%1) = sum(c, v_xa.l(r,c,"gov")*p_res(r,c,"gov","p",%1)/v_yg.l(r) * p_res(r,c,"gov","p",%1) );
   p_res(r,"inv","p","v",%1) = sum(c, v_xa.l(r,c,"inv")*p_res(r,c,"inv","p",%1)/v_ys.l(r) * p_res(r,c,"inv","p",%1) );
   p_res(r,"hou","p","v",%1) = sum(c, v_xa.l(r,c,"hou")*p_res(r,c,"hou","p",%1)/v_yc.l(r) * p_res(r,c,"hou","p",%1) );

   p_res(r,"gov","e","v",%1) = v_yg.l(r);
   p_res(r,"inv","e","v",%1) = v_ys.l(r);
   p_res(r,"hou","e","v",%1) = v_yc.l(r);
*
*  --- Equivalent variation: expenditure to reach same utility
*      as in current simulation (reflects changes in prices and income)
*      under benach market prices minus benchmark expenditures
*
   p_res(r,"gov","ev","v",%1) =
       + p_res(r,"gov","u","v",%1)
         * prod(c $ p_alphaa(r,c,"gov"),
               (1/p_alphaa(r,c,"gov"))**p_alphaa(r,c,"gov") )
     - p_res(r,"gov","e","v","bench");

   p_res(r,"inv","ev","v",%1) =
       + p_res(r,"inv","u","v",%1)
         * prod(c $ p_alphaa(r,c,"inv"),
               (1/p_alphaa(r,c,"inv"))**p_alphaa(r,c,"inv") )
     - p_res(r,"inv","e","v","bench");


   p_res(r,"tot","u","v",%1)
       = prod(dem, (p_res(r,dem,"e","v",%1)/p_res(r,dem,"p","v",%1))**p_phi(r,dem) );

   p_res(r,"tot","u1","v",%1)
       = v_regy.l(r)*prod(dem, (p_phi(r,dem)/p_res(r,dem,"p","v",%1))**p_phi(r,dem) );

   p_res(r,"tot","ev","v",%1)
       = p_res(r,"tot","u","v",%1)
         * prod(dem,
             (p_res(r,dem,"p","v","bench")/p_phi(r,dem))**p_phi(r,dem) )
        - p_res(r,"regy","v","v","bench");

   p_res(r,"hou","ev","v",%1) =  p_res(r,"tot","ev","v",%1)
                               -  p_res(r,"gov","ev","v",%1)
                               -  p_res(r,"inv","ev","v",%1);

   p_res(r,f,s,"q",%1)  = v_xf.l(r,f,s);
   p_res(r,"q",s,"q",%1) = v_x.l(r,s);
   p_res(r,f,s,"p",%1) = v_pf.l(r,f);

   p_res(r,c,aa,"q",%1) = v_xa.l(r,c,aa);
   p_res(r,c,aa,"p",%1) = v_px.l(r,c) * (1+p_oTax(r,c));

   p_res(r,"gov",s,"q",%1) = SUM(s_to_c(s,c), v_x.l(r,s) * v_px.l(r,c) * p_oTax(r,c));
   p_res(r,"gov",s,"p",%1) = SUM(s_to_c(s,c), v_px.l(r,c) * (1+p_oTax(r,c)));

   p_res(r,"gov",f,"q",%1) = SUM( (s),    v_xf.l(r,f,s) * v_pf.l(r,f) * p_fTax(r,f));
   p_res(r,"gov",f,"p",%1) = v_pf.l(r,f);

   p_res(r,rows,cols,"v",%1) = p_res(r,rows,cols,"p",%1) * p_res(r,rows,cols,"q",%1);

   p_res(r,brows,"revenue","v",%1) = sum(bcols,p_res(r,brows,bcols,"v",%1));
   p_res(r,"expend",bCols,"v",%1)  = sum(brows,p_res(r,brows,bcols,"v",%1));

  $$if not errorfree $abort Compilation error after file: %system.fn%
  if ( execerror, abort "Run-Time error in file: %system.fn%, line: %system.incline%");
