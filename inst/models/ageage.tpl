
DATA_SECTION

  init_int nobs
  init_vector age(1,nobs)
  init_vector ape(1,nobs)
  init_vector n(1,nobs)

INITIALIZATION_SECTION

  sigma1  0.5;
  sigma2  5;

PARAMETER_SECTION

  init_number sigma1(1)
  init_number sigma2(2)

  vector Perc_Corr(1,nobs)
  vector Perc_Corr1(1,nobs)
  vector Perc_Corr2(1,nobs)
  vector Phat(1,nobs)
  sdreport_vector sigma_a(1,nobs)
  number sigma_inc
  vector RSS(1,nobs)

  objective_function_value f

PROCEDURE_SECTION

  get_A_SD_est();
  evaluate_the_objective_function();

FUNCTION get_A_SD_est

  sigma_inc = (sigma2-sigma1)/(age(nobs)-age(1));
  sigma_a(1) = sigma1;
  for (int i=2;i<=nobs;i++){
   sigma_a(i) = sigma1+(age(i)-age(1))*sigma_inc;}

  for (int i=1;i<=nobs;i++){
  Perc_Corr(i) = cumd_norm(0.5/sigma_a(i))-cumd_norm((-0.5)/sigma_a(i));
  Perc_Corr1(i) = cumd_norm((-0.5)/sigma_a(i))-cumd_norm((-1.5)/sigma_a(i));
  Perc_Corr2(i) = cumd_norm((-1.5)/sigma_a(i))-cumd_norm((-2.5)/sigma_a(i));}

  Phat = square(Perc_Corr)+2*square(Perc_Corr1)+2*square(Perc_Corr2);

TOP_OF_MAIN_SECTION
    gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);

RUNTIME_SECTION
    convergence_criteria 1e-7,1e-7

FUNCTION evaluate_the_objective_function
  RSS.initialize();

  for (int i=1;i<=nobs;i++){
   if(n(i)>0){
   RSS(i) = pow(n(i),0.5)*square(Phat(i)-ape(i));}}

  f = sum(RSS);

