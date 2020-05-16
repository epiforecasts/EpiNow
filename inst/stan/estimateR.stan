data {
  int t; // number of time steps
  int k; // number of interval and case samples
  int n; // length of interval distributions
  int <lower = 0> obs_imported[t, k]; // imported cases
  int <lower = 0> obs_local[t, k]; // local cases
  int window; // length of window
  real intervals[n, k]; // matrix of different interval distributions
  real <lower = 0> r_mean;
  real <lower = 0> r_sd;
}

transformed data{
  // Set up transformed data objects
  vector[t] infectiousness[k];
  real r_alpha; //alpha parameter of the R gamma prior
  real r_beta;  //beta parameter of the R gamma prior

  // calculate alpha and beta for gamma distribution
  r_alpha = (r_mean / r_sd)^2;
  r_beta = (r_sd^2) / r_mean;
  
  // Calculate infectiousness at each timestep for each sample in turn
  for (j in 1:k){
      // Initialise infectiousness as zero initially
      infectiousness[j] = rep_vector(0, t);
      for (s in 2:t){
         for (i in 1:(min((s - 1), n - 1))){
           infectiousness[j][s] += (obs_imported[s - i, j] + obs_local[s - i, j]) * intervals[i + 1, j];
      }
    }
  }
  print(infectiousness)
}

parameters{
  vector<lower=0>[t] R[k]; // Effective reproduction number over time
  real <lower = 0> phi[k]; // Dispersion of negative binomial distribution
}

model {
    
  for (j in 1:k) {
    R[j] ~ gamma(r_alpha, r_beta); // Prior  on Rt
    phi[j] ~ exponential(0.5); //Prior on Phi
  }
  
  print(R)
  print(phi)
  
  
 //Build likelihood across all samples
 for(j in 1:k) {
  for (s in (window + 1):t){
    for (i in (s - window + 1):s){
      target += neg_binomial_2_lpmf(obs_local[i, j] | R[j][s] * infectiousness[j][i], phi[j]);
    }
   }
 }

}

