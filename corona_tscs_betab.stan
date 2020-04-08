// Coronavirus tracking model 
// Robert Kubinec
// New York University Abu Dhabi
// March 20, 2020
functions{
  vector setstep(vector col_var) {
    
    vector[rows(col_var)] diff_var = col_var + (max(fabs(col_var)) * sqrt(0.0000007)) - col_var;
  
    return diff_var;
  }
}
data {
    int time_all;
    int num_country;
    int cases[num_country,time_all];
    int tests[num_country,time_all];
    int time_outbreak[num_country,time_all];
    int S; // number of suppression measures
    matrix[time_all,3] ortho_time;
    matrix[num_country,S] suppress;
    vector[time_all] count_outbreak;
    matrix[num_country,time_all] time_outbreak_center;
    int country_pop[num_country];
    real phi_scale; // prior on how much change there could be in infection rate over time
}
transformed data {
  matrix[num_country,time_all] time_outbreak_trans1; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,time_all] time_outbreak_trans2; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,time_all] time_outbreak_trans3; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,3] time_array[time_all]; 
  
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
}
parameters {
  vector[3] poly; // polinomial function of time
  real<lower=0> finding; // difficulty of identifying infected cases 
  real<lower=0> world_infect; // infection rate based on number of travelers
  row_vector[S] suppress_effect[2]; // suppression effect of govt. measures, cannot increase virus transmission rate
  vector<lower=0>[num_country] country_test_raw; // unobserved rate at which countries are willing to test vs. number of infected
  // we assume that as infection rates increase, more tests will be conducted
  vector[2] alpha; // other intercepts
  vector<lower=0>[2] phi; // shape parameter for infected
  real<lower=0> sigma_test_raw; // estimate of between-state testing heterogeneity
}
transformed parameters {

  matrix[num_country,time_all] num_infected_high; // modeled infection rates for domestic transmission
  
  for(t in 1:time_all) {
      //real num_low;
      num_infected_high[,t] = alpha[2] + time_array[t]*poly + 
                                        world_infect*count_outbreak[t] +
                                        (suppress_effect[1]*suppress')' +
                                        + ((suppress_effect[2]*suppress') .* time_outbreak_trans1[,t]')';
  }

  
}
model {
  
  poly ~ normal(0,5); // could be large
  world_infect ~ normal(0,1);
  alpha ~ normal(0,10); // this can reach extremely low values
  phi ~ exponential(phi_scale);
  for(i in 1:2)
    suppress_effect[i] ~ normal(0,2);
  
  finding ~ exponential(.1);
  sigma_test_raw ~ exponential(.1);
  country_test_raw ~ exponential(.1); // more likely near the middle than the ends
  
  // first model probability of infection
  
  //next model the true infection rate as a function of time since outbreak
  for (t in 2:time_all) {
    
    vector[num_country] mix_prop = inv_logit(num_infected_high[,t]);
    // locations for cases and tests
    vector[num_country] mu_cases = inv_logit(-2.19 + finding*num_infected_high[,t]);
    vector[num_country] mu_tests = inv_logit(alpha[1] + country_test_raw .* num_infected_high[,t]);
    
    tests[,t] ~ beta_binomial(country_pop,mu_tests*phi[1],(1-mu_tests)*phi[1]);
    cases[,t] ~ beta_binomial(tests[,t],mu_cases*phi[2],(1-mu_cases)*phi[2]);
  
  }

  
}
generated quantities {
  
  //   vector[S] suppress_margin;
  //   matrix[time_all,rows(suppress)] suppress_margin_time;
  // 
  // for(s in 1:S) {
  //   
  //   //matrix[rows(suppress),cols(suppress)] suppress_high = suppress;
  //   //matrix[rows(suppress),cols(suppress)] suppress_low = suppress;
  //   //matrix[rows(suppress),rows(count_outbreak)] y_high;
  //   //matrix[rows(suppress),rows(count_outbreak)] y_low;
  //   //matrix[rows(suppress),rows(count_outbreak)] suppress_margin_time;
  //   
  //   
  //   //suppress_high[,s] = suppress[,s] + setstep(suppress[,s]);
  //   //suppress_low[,s] = suppress[,s] - setstep(suppress[,s]);
  //   
  //   
  //   for(t in 1:time_all) {
  //     
  //       // y_high[,t] =  alpha[2] + country_int + 
  //       //                                 time_array[t]*poly + 
  //       //                                 world_infect*count_outbreak[t] +
  //       //                                 (suppress_effect[1]*suppress_high')' +
  //       //                                 ((suppress_effect[2]*suppress_high') .* time_outbreak_center[,t]')';
  //       //                                 
  //       // y_low[,t] = alpha[2] + country_int + 
  //       //                                 time_array[t]*poly + 
  //       //                                 world_infect*count_outbreak[t] +
  //       //                                 (suppress_effect[1]*suppress_low')' +
  //       //                                 ((suppress_effect[2]*suppress_low') .* time_outbreak_center[,t]')';
  //                                       
  //                                       
  //       suppress_margin_time[t,s] = mean(inv_logit(num_infected_high[,t]) .* (suppress_effect[1,s] + suppress_effect[2,s] * time_outbreak_center[,t]));
  //       //                                 
  //   }
  //   
  //   suppress_margin[s] = mean(inv_logit(to_vector(suppress_margin_time)));
  // }
  
  
}

