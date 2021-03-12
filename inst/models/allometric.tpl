
DATA_SECTION

  init_int nlengths;
  init_vector lengths(1,nlengths);
  init_vector Wbar_obs(1,nlengths);
  init_vector SD_Wbar(1,nlengths);

INITIALIZATION_SECTION

  alpha  0.0001;
  beta   3;

PARAMETER_SECTION

  init_bounded_number alpha(0.000000000000000001,1,1);
  init_number beta(1);

  vector Wbar_est(1,nlengths);
  vector yvar(1,nlengths);
  vector yconst(1,nlengths);
  vector RSS(1,nlengths);

  objective_function_value jnll;

PROCEDURE_SECTION

  get_Wbar_est();
  evaluate_the_objective_function();

FUNCTION get_Wbar_est

  for (int i=1;i<=nlengths;i++){
   Wbar_est(i) = alpha*pow(lengths(i),beta);
   yvar(i) = log(1.+square(SD_Wbar(i))/square(Wbar_obs(i)));
   yconst(i) = log(2.0*M_PI*yvar(i)*square(Wbar_obs(i)));}

FUNCTION evaluate_the_objective_function

  for (int i=1;i<=nlengths;i++){
   RSS(i) = 0.5*(yconst(i)+square(log(Wbar_est(i))-log(Wbar_obs(i)))/yvar(i));}

  jnll = sum(RSS);
