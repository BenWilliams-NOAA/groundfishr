
DATA_SECTION

  init_int nages;
  init_vector ages(1,nages);
  init_vector Wbar_obs(1,nages);
  init_vector SD_Wbar(1,nages);

// Read parameter phases from the control file
  !! ad_comm::change_datafile_name("wvbl.ctl");
  init_int    ph_Winf;
  init_int    ph_k;
  init_int    ph_t0;
  init_int    ph_beta;

PARAMETER_SECTION

  init_number Winf(ph_Winf);
  init_bounded_number k(0.0001,0.777,ph_k);
  init_bounded_number t0(-3,3,ph_t0);
  init_number beta(ph_beta);
  vector Wbar_est(1,nages);
  vector yvar(1,nages);
  vector yconst(1,nages);
  vector RSS(1,nages);
  objective_function_value jnll;

PROCEDURE_SECTION

  get_Wbar_est();
  evaluate_the_objective_function();

FUNCTION get_Wbar_est

  for (int i=1;i<=nages;i++){
   Wbar_est(i) = Winf*pow((1-exp(-1.0*k*(ages(i)-t0))),beta);
   yvar(i) = log(1.+square(SD_Wbar(i))/square(Wbar_obs(i)));
   yconst(i) = log(2.0*M_PI*yvar(i)*square(Wbar_obs(i)));}

FUNCTION evaluate_the_objective_function

  for (int i=1;i<=nages;i++){
   RSS(i) = 0.5*(yconst(i)+square(log(Wbar_est(i))-log(Wbar_obs(i)))/yvar(i));}

  jnll = sum(RSS);

REPORT_SECTION
  report << "Winf "<<Winf<<endl;
  report << "k "<<k<<endl;
  report << "t0 "<<t0<<endl;
  report << "beta "<<beta<<endl;
  report << "Obj_fun "<<jnll<<endl;

GLOBALS_SECTION
  #include <admodel.h>
  #undef REPORT
  #define write_R(object) mysum << #object "\n" << object << endl;
  ofstream mysum("Est_out.rep");

FINAL_SECTION

  write_R(ages);
  write_R(Wbar_obs);
  write_R(Wbar_est);
  mysum.close();
