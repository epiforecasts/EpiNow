data {
  int t; // number of time steps
  int k; // number of interval and case samples
  int n; // length of interval distributions
  int w; //Number of windows to evaluate
  int wait_time; //Time to wait before starting to estimate
  int <lower = 0> obs_imported[t, k]; // imported cases
  int <lower = 0> obs_local[t, k]; // local cases
  int windows[w]; // length of window
  int model_type; //Type of model: 1 = Poisson otherwise negative binomial
  real intervals[n, k]; // matrix of different interval distributions
  real <lower = 0> r_mean;
  real <lower = 0> r_sd;
  real <lower = 0> phi_mean;
  real <lower = 0> phi_sd;
}

transformed data{
  vector[k] infectiousness[t]; //Infectiousness at any time t vectorised across samples
  real r_alpha; //alpha parameter of the R gamma prior
  real r_beta;  //beta parameter of the R gamma prior
  
  // calculate alpha and beta for gamma distribution
  r_alpha = (r_mean / r_sd)^2;
  r_beta = (r_sd^2) / r_mean;
  
      
  // Calculate infectiousness at each timestep for each sample in turn
  for (s in 2:t){
    // Initialise infectiousness as zero initially
    infectiousness[s] = rep_vector(0.0, k);
    
      for (j in 1:k){
         for (i in 1:(min((s - 1), n - 1))){
           infectiousness[s, j] += (obs_imported[s - i, j] + obs_local[s - i, j]) * intervals[i + 1, j];
      }
    
     //If infectiousness is ever zero set to be nearly zero to avoid sampling issues
     if (infectiousness[s, j] == 0) {
       infectiousness[s, j] = 0.0000001;
     }
     
    }
  }
}

parameters{
  vector<lower = 0>[t - wait_time] R[w]; // Effective reproduction number over time
  real <lower = 0> phi; // Dispersion of negative binomial distribution
  simplex[w] weights[t - wait_time]; //Weights of each window
}


model {
  //Log likelihood across windows
  real avg_R;
  vector[w] lps;
  vector[k] pred_cases;
  vector[k] one_day_pred_cases;
  
  // Set up priors on parameters
   for (l in 1:w) {
     R[l] ~ gamma(r_alpha, r_beta); // Prior  on Rt
   }
  
  if (phi_mean == 0) {
    phi ~ exponential(1);
  }else{
    phi ~ normal(phi_mean, phi_sd) T[0,]; //Prior on Phi
  }
  
 //Build likelihood each time point and window starting when all windows have data
 for (s in (wait_time + 1):(t)){
   //Initialise mixture model and start window at 0
   lps = log(weights[s - wait_time]);
    for (l in 1:w) {
      for (i in (s - windows[l] + 1):s) {
         //Likelihood over each window - vectorised over the no. of samples
         pred_cases = R[l][s - wait_time] * infectiousness[i];
         if (model_type == 1) {
           target += poisson_lpmf(obs_local[i] | pred_cases);
         }else{
           target += neg_binomial_2_lpmf(obs_local[i] | pred_cases, phi);
         }
      }

      //One-day ahead likelihood for window mixture model
      one_day_pred_cases = R[l][s - wait_time] * infectiousness[s];
      if (model_type == 1) {
       lps[l] +=  poisson_lpmf(obs_local[s] |  one_day_pred_cases);
      }else{
       lps[l] +=  neg_binomial_2_lpmf(obs_local[s] | one_day_pred_cases, phi);
      }
    }
    //Mixture model of windows
    target += log_sum_exp(lps);
  }
}

