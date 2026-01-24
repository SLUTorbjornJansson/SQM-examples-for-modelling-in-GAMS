********************************************************************************
$ontext

   Modeling of Agri-Ecological Systems

   GAMS file : herd_dyn.GMS

   @purpose  : Herd dynamics and feed cost min. in farm scale modelling
               Compare comparative-static, fully dynamic and rec-dyn solution
               Some checks and tests implemented
   @author   : Wolfgang Britz
   @date     : 14.03.17
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy : stand-alone

$offtext
********************************************************************************
  $$offlisting

* --- Initialize error catching mechanism
  $$batinclude "../shared/assert_no_problem.gms" init

* --------------------------------------------------------------------------------------
*
*   Set definitions
*
* --------------------------------------------------------------------------------------

  set t "years" / t1*t5/;

  alias(t,t1);
  set curt(t);

  scalar lag / 1 /;

  set herds / cow     "Adult cow"
              mCalv   "Male calf, up to 1 year"
              fCalv   "Female calf, up to 1 year"
              bull    "Bull > 1 year"
              heifR   "Heifers for raising, > 1 year"
              heifF   "Heifers for fattening, > 1 year"
              slgtCow "Slaughtered cow"
            /;
  alias(herds,herds1,herds2);

  set reqs  "Animal requirement" / enne "Net energy lactation, min"
                                   crpr "Crude protein, min"
                                   drmx "Dry matter, max"/;
  set feeds / concCattle,grasSil,grasPast,maizSil /;

  $$if not errorfree $abort Compilation errors related to set definitions input, in file: %system.fn% , before line: %system.incline%
  if ( execerror, abort "Run-Time error related to set definitions, in file: %system.fn%, line: %system.incline%");

* --------------------------------------------------------------------------------------
*
*   Data input
*
* --------------------------------------------------------------------------------------

  parameter herd_t_herd(herds,herds) "Transition coefficient from herd in t-1 to herd in t" /

       cow.mCalv        0.50
       cow.fCalv        0.50
       cow.cow          0.80
       cow.slgtCow      0.20

       fCalv.heifR      0.40
       fCalv.heiff      0.60
       mCalv.bull       1.00
       heifR.cow        1.00

  /;

  p_problem1D(herds) $ ((sum(herds1,herd_t_herd(herds,herds1)) ne 1) $ (not sameas(herds,"cow"))) = sum(herds1,herd_t_herd(herds,herds1));
  $$batinclude "../shared/assert_no_problem.gms" p_problem1D "Transition coefficient of at least one herd don't add to unity, in file: %system.fn%, line: %system.incline%"

  parameter p_gm(herds) "Gross margins per year and animal, without feed cost" /
     cow       2000
     mCalv     -50
     fCalv     -50
     bull      1000
     heifR     -100
     heifF      800
     slgtCow    200
  /;
* Note: Raising animal processes (calves, heifers for raising) have no marketable output and therefore negative gross margins.

  parameter p_priceCows "Price for buying a young cow ready to enter the herd" / 2000 /;

  table p_cont(feeds,reqs) "Requirement contents related to fresh weight per kg"
                   enne     crpr    drmx
   concCattle      7.61    0.18   -0.90
   grasSil         2.09    0.054  -0.35
   grasPast        1.10    0.038  -0.18
   maizSil         2.26    0.028  -0.35
  ;
* Note: The negative values refer to maximal requirements

  p_problem2D(feeds,reqs) $ (p_cont(feeds,reqs) eq 0) = eps;
  $$batinclude "../shared/assert_no_problem.gms" p_problem2D "Missing requirement contents (check for eps), in file: %system.fn%, line: %system.incline%"


  parameter p_feedPrice(feeds) "Price of feed per ton" /
   concCattle    200
   grasSil        28
   grasPast       26
   maizSil        32
  /;

  p_problem1D(feeds) $ (p_feedPrice(feeds) le 0) = p_feedPrice(feeds) + eps;
  $$batinclude "../shared/assert_no_problem.gms" p_problem1D "Negative or missing feed prices, in file: %system.fn%, line: %system.incline%"

  parameter p_GHGPerFeed(feeds) "GHG emissions Co2eq in ton of each feed per ton" /
   concCattle     10
   grasSil         3
   grasPast       1.5
   maizSil        3.5
  /;

  p_problem1D(feeds) $ (p_GHGPerFeed(feeds) le 0) = p_GHGPerFeed(feeds) + eps;
  $$batinclude "../shared/assert_no_problem.gms" p_problem1D "Negative or missing GHG emission coefficient for feeds, in file: %system.fn%, line: %system.incline%"

  table p_reqs(herds,reqs) "Yearly requirement per animal"

               enne       crpr        drmx
  heiff   16588.348     297.060   -2624.879
  heifr   16588.348     297.060   -2624.879
  bull    16588.348     297.060   -2624.879
  fCalv    8546.740     189.980   -1236.056
  mCalv    8035.721     196.490   -1103.221
  cow     48003.056     977.055   -7665.000
  ;
* Note: The negative values refer to maximal requirements

  p_problem2D(herds,reqs) $ ((p_reqs(herds,reqs) eq 0) and (not sameas(herds,"slgtCow"))) = eps;
  $$batinclude "../shared/assert_no_problem.gms" p_problem2D "Missing requirements (check for eps), in file: %system.fn%, line: %system.incline%"

  parameter p_res(*,*,*);

  $$if not errorfree $abort Compilation errors related to data input, in file: %system.fn% , before line: %system.incline%
  if ( execerror, abort "Run-Time error related to data input, in file: %system.fn%, line: %system.incline%");

* -------------------------------------------------------------------------------------
*
*  Model equations and variables
*
* -------------------------------------------------------------------------------------

  equations
    e_herd(herds,t)         "Herd dynamics"
    e_reqs(herds,reqs,t)    "Requirement constraints"
    e_GHG                   "Maximum GHG emissions linked to feed use"
    e_gm(t)                 "Yearly gross margin"
    e_obje                  "Objective function: average farm gross margin per year"
  ;

  positive variables
    v_herd(herds,t)                "Average # of animals in year"
    v_buyCows(t)                   "Buy a cow at beginning of year"
    v_sellCows(t)                  "Sell a cow at beginning of year"
    v_feeding(herds,feeds,t)       "Feeding in ton fresh weight"
    v_ghg(t)                       "Yearly GHG emissions linked to feed use"
  ;

  variables
    v_gm                           "Farm gross margin, per year"
    v_obje                         "Farm gross margin, average per year"
  ;
*
* --- herd dynamics (current herd size must fit transition from the same of other herds in the previous year,
*                     considering buying and selling in case of adult cows)
*
  e_herd(herds,curt(t)) ..

     v_herd(herds,t) =E=  sum(herds1 $ herd_t_herd(herds1,herds),
                            v_herd(herds1,t-lag) * herd_t_herd(herds1,herds))

                        + v_buyCows (t) $ sameas(herds,"cow")
                        - v_sellCows(t) $ sameas(herds,"cow");
*
* --- animal requirement constraints
*
  e_reqs(herds,reqs,curt(t)) ..

     v_herd(herds,t) * p_reqs(herds,reqs)
        =L= sum(feeds, v_feeding(herds,feeds,t)*p_cont(feeds,reqs)*1000);
*
* --- GHG emissions linekd to feed use
*
  e_GHG(curT(t)) ..

     v_ghg(t)
        =E= sum( (herds,feeds), v_feeding(herds,feeds,t)*p_GHGPerFeed(feeds));
*
* --- gross margins in each year (from herds, buying feeds and cows, and selling cows)
*
  e_gm(curt(t)) ..

      v_gm(t) =E=    sum( herds,       v_herd(herds,t)*p_gm(herds))
                   - sum( (herds,feeds), v_feeding(herds,feeds,t) * p_feedPrice(feeds))
                   - v_buyCows(t)  * p_priceCows
*                  --- assume that the price for selling a productive cow (not for slaughter) is 75% of the price
*                      of buying a new one
                   + v_sellCows(t) * p_priceCows * 0.75;
*
* --- farm gross margin (average over simulation horizon curT)
*
  e_obje ..

    v_obje * card(curT) =E= sum(curt, v_gm(curT));

  model m_herd / all /;
  m_herd.solvelink=5;
  m_herd.solprint =1;
  option limcol = 0;
  option limrow = 0;

  $$if not errorfree $abort Compilation errors related to model definition, in file: %system.fn% , before line: %system.incline%
  if ( execerror, abort "Run-Time error related to model definition, in file: %system.fn%, line: %system.incline%");
*
* --- maximal herd size
*     Note: In this simple model focusing on the herd dynamics and feeding, the upper limit captures the impact of other constraints
*           in more evolved models, such as stable sizes, grazing areas, labor endowments etc.
*
  v_herd.up("cow",t)    =  100;
*
* --- Comparative static solution: Only one year, not lags in herd dynamics = steady state solution
*
  option kill=curT;
  curT("t1") = YES;
  lag = 0;
  solve m_herd using LP maximizing v_obje;
  abort $ m_herd.sumInfes " Comparative-static base run ended with infeasibilities, in file: %system.fn%, line: %system.incline%";

  $$if not errorfree $abort Compilation errors after comparative-static solve, in file: %system.fn% , before line: %system.incline%
  if ( execerror, abort "Run-Time error related comparative-static solve, in file: %system.fn%, line: %system.incline%");

  $$batinclude 'store_res_herd.gms' "'comp_stat'" curT curT
*
* --- market intelligence tests with comp-stat version: increase gross margin of one herds and check that
*     (1) profit increases
*     (2) size of herd with higher gross does not drop (beware: with a LP, the herd might not increase)
*
  loop(herds2,

      p_gm(herds2) = p_gm(herds2) + 50;
      solve m_herd using LP maximizing v_obje;
      abort $ m_herd.sumInfes " Test run for increased gross margins ended with infeasibilities, in file: %system.fn%, line: %system.incline%";

      p_problem1D(herds2) $ (v_obje.l lt p_res("comp_stat","obje","t1")) = v_obje.l - p_res("comp_stat","obje","t1");
      $$batinclude "../shared/assert_no_problem.gms" p_problem1D "Objective drops despite increased gross margin, in file: %system.fn%, line: %system.incline%"

      p_problem1D(herds2) $ (v_herd.l(herds2,"t1") lt p_res("comp_stat",herds2,"t1")) = v_obje.l - p_res("comp_stat","obje","t1");
      $$batinclude "../shared/assert_no_problem.gms" p_problem1D "Herd with increased gross margins drops in test solve, in file: %system.fn%, line: %system.incline%"

      p_gm(herds2) = p_gm(herds2) - 50;
  );
*
* --- Fully dynamic version: Solve simultaneously for the whole time horizon
*
  lag = 1;
  curt(t) = YES;
  solve m_herd using LP maximizing v_obje;
  abort $ m_herd.sumInfes "Fully dynamic base run ended with infeasibilities, in file: %system.fn%, line: %system.incline%";

  $$if not errorfree $abort Compilation errors after fully dynamic solve, in file: %system.fn% , before line: %system.incline%
  if ( execerror, abort "Run-Time error related after fully dynamic solve, in file: %system.fn%, line: %system.incline%");

  $$batinclude 'store_res_herd.gms' "'full_Dyn'" t t
*
* --- Recursive-dynamics over one year (this will not open the farm, too myopic)
*
  loop(t1,
      option kill=curT;
      curT(t1)   = YES;
      solve m_herd using LP maximizing v_obje;
      abort $ m_herd.sumInfes "Recursive-dynamic base run ended with infeasibilities, in file: %system.fn%, line: %system.incline%";
      v_herd.fx(herds,t1) = v_herd.l(herds,t1);
      $$batinclude 'store_res_herd.gms' "'rec_Dyn1'" t1 t
  );
*
* --- Recursive-Dynamic over two years (sufficiently long forwarded looking period, should generate same results as fully dynamic run)
*     Note that we need to remove the lower and upper bounds for herds that were fixed in the loop over t1 above. As the upper bounds
*     are all infinite after the kill statement, we need the upper limit for the cow herd again.
*
  option kill=v_herd.lo;
  option kill=v_herd.up;
  v_herd.up("cow",t)    =  100;

  loop(t1,
      option kill=curT;
      curT(t1)   = YES;
      curT(t1+1) = YES;
      solve m_herd using LP maximizing v_obje;
      abort $ m_herd.sumInfes " Recursive-dynamic base run ended with infeasibilities, in file: %system.fn%, line: %system.incline%";
      v_herd.fx(herds,t1) = v_herd.l(herds,t1);
      $$batinclude 'store_res_herd.gms' "'rec_Dyn2'" t1 t
  );
*
* --- check that fully dynamic and rec-dyn over 2 years give the same herd sizes
*
  p_problem2D(herds,t) $ (p_res('rec_dyn2',herds,t) ne p_res('full_dyn',herds,t)) = p_res('rec_dyn2',herds,t) - p_res('full_dyn',herds,t);
  $$batinclude "../shared/assert_no_problem.gms" p_problem2D "Fully dynamic and rec-dyn over 2 year give different herd sizes, in file: %system.fn%, line: %system.incline%"

  display p_res;

  $$if not errorfree $abort Compilation errors after recursive-dynamic solve, in file: %system.fn% , before line: %system.incline%
  if ( execerror, abort "Run-Time error related after recursive-dynamic solve, in file: %system.fn%, line: %system.incline%");
