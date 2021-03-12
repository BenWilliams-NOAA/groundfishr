
DATA_SECTION

  init_int nages;
  init_vector ages(1,nages);
  init_vector Lbar_obs(1,nages);
  init_vector SD_Lbar(1,nages);

INITIALIZATION_SECTION

  Linf   50;
  k      0.1;
  t0     0.1;

PARAMETER_SECTION

  init_number Linf(1);
  init_number k(2);
  init_number t0(3);
  vector Lbar_est(1,nages);
  vector yvar(1,nages);
  vector yconst(1,nages);
  vector RSS(1,nages);
  objective_function_value jnll;

PROCEDURE_SECTION

  get_Lbar_est();
  evaluate_the_objective_function();

FUNCTION get_Lbar_est

  for (int i=1;i<=nages;i++){
   Lbar_est(i) = Linf*(1-exp(-1.0*k*(ages(i)-t0)));
   yvar(i) = log(1.+square(SD_Lbar(i))/square(Lbar_obs(i)));
   yconst(i) = log(2.0*M_PI*yvar(i)*square(Lbar_obs(i)));}

FUNCTION evaluate_the_objective_function

  for (int i=1;i<=nages;i++){
   RSS(i) = 0.5*(yconst(i)+square(log(Lbar_est(i))-log(Lbar_obs(i)))/yvar(i));}

  jnll = sum(RSS);

REPORT_SECTION
  report << "Linf "<<Linf<<endl;
  report << "k "<<k<<endl;
  report << "t0 "<<t0<<endl;
  report << "Obj_fun "<<jnll<<endl;

GLOBALS_SECTION
  #include <admodel.h>
  #undef REPORT
  #define write_R(object) mysum << #object "\n" << object << endl;
  ofstream mysum("Est_out.rep");

FINAL_SECTION

  write_R(ages);
  write_R(Lbar_obs);
  write_R(Lbar_est);
  mysum.close();
