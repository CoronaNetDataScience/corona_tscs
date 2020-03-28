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
    int country_pop[num_country];
}
transformed data {
  matrix[num_country,time_all] time_outbreak_trans1; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,time_all] time_outbreak_trans2; // convert raw time numbers to ortho-normal polynomials
  matrix[num_country,time_all] time_outbreak_trans3; // convert raw time numbers to ortho-normal polynomials
  vector[time_all] count_outbreak = rep_vector(0,time_all); // number of countries with infection at time point t
  matrix[num_country,3] time_array[time_all]; 
  matrix[num_country,time_all] time_outbreak_center;
  
  for(t in 1:time_all) {
    for(n in 1:num_country)
        count_outbreak[t] += cases[n,t] > 0 ? 1 : 0;
  }
  
  // mean-center & standardize
  
  count_outbreak = (count_outbreak - mean(count_outbreak))/sd(count_outbreak);
  
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
    time_outbreak_center[n,] = to_vector(time_outbreak[n,])' - mean(to_vector(time_outbreak[n,]));
  }
    
}
parameters {
  vector[3] poly; // polinomial function of time
  real<lower=0> finding; // difficulty of identifying infected cases 
  real<lower=0> world_infect1; // rate of outbreak crossing borders
  real<lower=0> world_infect2; // effect of world-wike infection on domestic rate
  vector<upper=0>[2] suppress_effect; // suppression effect of govt. measures, cannot increase virus transmission rate
  vector<lower=0>[num_country] country_test; // unobserved rate at which countries are willing to test vs. number of infected
  // we assume that as infection rates increase, more tests will be conducted
  real<upper=0> alpha_high; // don't want this too high as it suggests that the number of sick people is relatively constant
  vector[3] alpha; // other intercepts
  vector<lower=0>[2] phi_raw; // shape parameter for infected
  vector[num_country-1] country_int_free; // varying intercepts by country - 1 for identification
}
transformed parameters {
  vector[2] phi = phi_raw*30; // need to rescale this parameter as it can get very large
  vector[num_country] country_int = append_row(0,country_int_free); // not strictly necessary but centers around 0
  matrix<lower=0,upper=1>[num_country,time_all] num_infected_high; // modeled infection rates for domestic transmission
  matrix<lower=0,upper=1>[num_country,time_all] num_infected_low; // modeled infection rates for transmission from abroad
  
  
  num_infected_high[,1] = rep_vector(0,num_country);
  num_infected_low[,1] = rep_vector(0,num_country);
  

    for(t in 1:time_all) {
      real num_low;
      num_infected_high[,t] = inv_logit(alpha[1] + time_array[t]*poly + 
                                        suppress_effect[2]*suppress .* time_outbreak_center[,t]);
      num_low = inv_logit(alpha[2] + world_infect2*count_outbreak[t]);                               
      num_infected_low[,t] = rep_vector(num_low,num_country);
    }

  
}
model {
  
  poly ~ normal(0,10); // could be large
  world_infect1 ~ exponential(.1); 
  world_infect2 ~ exponential(.1);
  suppress_effect ~ normal(0,2);
  alpha ~ normal(0,10); // this can reach extremely low values
  alpha_high ~ normal(0,5);
  phi_raw ~ exponential(.1);
  
  finding ~ normal(0,2);
  country_int_free ~ normal(0,1);
  country_test ~ normal(0,3); // more likely near the middle than the ends
  
  // first model probability of infection
  
  //next model the true infection rate as a function of time since outbreak
  for (t in 2:time_all) {
    // probability of cases from which domestic transmission vs. foreign travel
    vector[num_country] pr_domestic = inv_logit(alpha[4] + country_int + 
                                                world_infect1*count_outbreak[t] + suppress_effect[1]*suppress);
    // combined mixture probability
    vector[num_country] mix_prop = exp(log(pr_domestic) + log(num_infected_high[,t])) +
                                                                exp(log1m(pr_domestic) + log(num_infected_low[,t]));
    // locations for cases and tests
    vector[num_country] mu_cases = inv_logit(-2.19 + finding*mix_prop);
    vector[num_country] mu_tests = inv_logit(alpha[3] + country_test .* mix_prop);
    
    tests[,t] ~ beta_binomial(country_pop,mu_tests*phi[1],(1-mu_tests)*phi[1]);
    cases[,t] ~ beta_binomial(tests[,t],mu_cases*phi[2],(1-mu_cases)*phi[2]);
  
  }

  
}
generated quantities {
    matrix[num_country,time_all] prob_state;
    matrix[num_country,time_all] overall_infect;

    
    for(t in 1:time_all) {
      for(n in 1:num_country) {
        // first calculate what the probability of domestic transmission is at a particular time point
        prob_state[n,t] = inv_logit(alpha[4] + country_int[n] + world_infect1*count_outbreak[t] + suppress_effect[1]*suppress[n]);
        // then calculate overall combined infection rate
        overall_infect[n,t] = prob_state[n,t]*num_infected_high[n,t] + 
                            (1-prob_state[n,t])*num_infected_low[n,t];
      }
    }
    
    

    
    
}

