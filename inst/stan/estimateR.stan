data {
  int t; // number of time steps
  int k; // number of serial interval and delay samples
  int n; // length of interval distributions
  int <lower = 0> obs_imported[t, k]; // imported cases
  int <lower = 0> obs_local[t, k]; // local cases
  int window; // length of window
  matrix[n, k] intervals; // matrix of different interval distributions
  real <lower = 0> r_mean;
  real <lower = 0> r_sd;
}

transformed data{
  // Set up transformed data objects
  matrix[t, k] infectiousness;
  real r_alpha; //alpha parameter of the R gamma prior
  real r_beta;  //beta parameter of the R gamma prior

  // Initialise infectiousness as zero everywhere
  infectiousness = rep_matrix(0, t, k);
  // calculate alpha and beta for gamma distribution
  r_alpha = (r_mean / r_sd)^2;
  r_beta = (r_sd^2) / r_mean;
  
  // Calculate infectiousness at each timestep for each sample in turn
  for (j in 1:k){
      for (s in 2:t){
         for (i in 1:(min((s - 1), n - 1))){
           infectiousness[s, j] += (obs_imported[s - i, j] + obs_local[s - i, j]) * intervals[i + 1, j];
      }
    }
  }
}

parameters{
  real <lower = 0> R[t]; // Effective reproduction number over time
  real <lower = 0> phi; // Dispersion of negative binomial distribution
}

model {
  R ~ gamma(r_alpha, r_beta); // Prior  on Rt
  phi ~ normal(0, 1) T[0,]; //Prior on Phi
  
  
 //Build likelihood across all samples for each   
 for(j in 1:k) {
  for (s in (window + 1):t){
    for (i in (s - window + 1):s){
      target += neg_binomial_2_lpmf(obs_local[i, j] | R[s] * infectiousness[i, j], 1 / sqrt(phi));
    }
   }
 }

}

