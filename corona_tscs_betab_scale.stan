// Coronavirus tracking model 
// Robert Kubinec
// New York University Abu Dhabi
// March 20, 2020

data {
    int time_all;
    int num_country;
    int cases[num_country,time_all];
    int tests[num_country,time_all];
    int time_outbreak[num_country,time_all];
    matrix[time_all,3] ortho_time;
    vector[num_country] suppress;
    vector[time_all] count_outbreak;
    int country_pop[num_country];
    real phi_scale; // prior on how much change there could be in infection rate over time
}
transformed data {
  matrix[num_country,time_all] time_outbreak_trans1; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,time_all] time_outbreak_trans2; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,time_all] time_outbreak_trans3; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,3] time_array[time_all]; 
  matrix[num_country,time_all] time_outbreak_center;
  
  for(t in 1:time_all) {
    for(n in 1:num_country) {
      if(time_outbreak[n,t]>0) {
        time_outbreak_trans1[n,t] = ortho_time[time_outbreak[n,t],1];
        time_outbreak_trans2[n,t] = ortho_time[time_outbreak[n,t],2];
        time_outbreak_trans3[n,t] = ortho_time[time_outbreak[n,t],3];
      } else {
        time_outbreak_trans1[n,t] = 0;
        time_outbreak_trans2[n,t] = 0;
        time_outbreak_trans3[n,t] = 0;
      }
    }
  }
  
  // make a new array of time indices
  
  for(t in 1:time_all) {
    time_array[t] = append_col(time_outbreak_trans1[,t],
                                append_col(time_outbreak_trans2[,t],
                                            time_outbreak_trans3[,t]));
  }
  
  // need a centered time vector
  
  for(n in 1:num_country) {
    time_outbreak_center[n,] = (to_vector(time_outbreak[n,])' - mean(to_vector(time_outbreak[n,])))/sd(to_vector(time_outbreak[n,]));
  }
    
}
parameters {
  vector[3] poly; // polinomial function of time
  real<lower=0> finding; // difficulty of identifying infected cases 
  real world_infect;// infection rate based on number of travelers
  vector<upper=0>[2] suppress_effect; // suppression effect of govt. measures, cannot increase virus transmission rate
  vector<lower=0>[num_country] country_test_raw; // unobserved rate at which countries are willing to test vs. number of infected
  // we assume that as infection rates increase, more tests will be conducted
  vector[2] alpha; // other intercepts
  vector<lower=0>[2] phi; // shape parameter for infected
  vector[num_country-1] country_int_free; // varying intercepts by country - 1 for identification
  real<lower=0> sigma_test_raw; // estimate of between-state testing heterogeneity
}
transformed parameters {
  vector[num_country] country_int = append_row(0,country_int_free); // not strictly necessary but centers around 0
  matrix[num_country,time_all] num_infected_high; // modeled infection rates for domestic transmission
  
  
  num_infected_high[,1] = rep_vector(0,num_country);
  
  for(t in 1:time_all) {
      //real num_low;
      num_infected_high[,t] = alpha[2] + country_int + 
                                        time_array[t]*poly + 
                                        world_infect*count_outbreak[t] +
                                        suppress_effect[1]*suppress + 
                                        suppress_effect[2]*suppress .* time_outbreak_center[,t];
  }

  
}
model {
  
  poly ~ normal(0,10); // could be large
  world_infect ~ normal(0,1);
  alpha ~ normal(0,10); // this can reach extremely low values
  phi ~ exponential(phi_scale);
  suppress_effect ~ normal(0,2);
  
  finding ~ exponential(.1);
  country_int_free ~ normal(0,3);
  sigma_test_raw ~ exponential(.1);
  country_test_raw ~ exponential(sigma_test_raw); // more likely near the middle than the ends
  
  // first model probability of infection
  
  //next model the true infection rate as a function of time since outbreak
  for (t in 2:time_all) {
    
    vector[num_country] mix_prop = inv_logit(num_infected_high[,t]);
    // locations for cases and tests
    vector[num_country] mu_cases = inv_logit(-2.19 + finding*num_infected_high[,t]);
    vector[num_country] mu_tests = inv_logit(alpha[1] + country_test_raw .* num_infected_high[,t]);
    
    tests[,t] ~ beta_binomial(country_pop,mu_tests*phi[1],(1-mu_tests)*phi[1]);
    cases[,t] ~ beta_binomial(tests[,t],mu_cases*phi[2],(1-mu_cases)*phi[2]);
    
    log(mix_prop) - log(mu_tests) ~ std_normal();
  
  }

  
}

