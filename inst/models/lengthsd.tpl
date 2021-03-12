DATA_SECTION

  init_int nobs
  init_vector age(1,nobs)
  init_vector L_SD_obs(1,nobs)
  init_vector n(1,nobs)

INITIALIZATION_SECTION

  alpha  0.005;
  beta   0.5;

PARAMETER_SECTION

  init_number alpha(1)
  init_number beta(2)

  vector L_SD_est(1,nobs)
  vector RSS(1,nobs)

  objective_function_value f

PROCEDURE_SECTION

  get_L_SD_est();
  evaluate_the_objective_function();

FUNCTION get_L_SD_est

  for (int i=1;i<=nobs;i++){
   L_SD_est(i) = alpha*log(age(i))+beta;}


TOP_OF_MAIN_SECTION
    gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);

RUNTIME_SECTION
    convergence_criteria 1e-7,1e-7

FUNCTION evaluate_the_objective_function

  for (int i=1;i<=nobs;i++){
   RSS(i) = pow(n(i),0.5)*square(log(L_SD_est(i))-log(L_SD_obs(i)));}

  f = sum(RSS);
