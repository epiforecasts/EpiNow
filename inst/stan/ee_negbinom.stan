data {
  int t; // number of time steps
  int k;
  int n;
  int <lower = 0> obs_imported[t, k]; // imported cases
  int <lower = 0> obs_local[t, k]; // local cases
  int tau; // length of window
  matrix[n, k] w;
}

transformed data{
  matrix[t, k] infectiousness;

 infectiousness[1,] = rep_row_vector(0, k);

 for(h in 1:k) {
  // Calculate infectiousness at each timestep
  for (s in 2:t){
    infectiousness[s, k] = 0;
    for (i in 1:(s - 1)){
      infectiousness[s, k] += (obs_imported[i, k] + obs_local[i, k]) * w[s - i, k];
    }
  }
 }

}

parameters{
  real <lower = 0> R[t]; // Effective reproduction number over time
  real <lower = 0> phi; // Dispersion of negative binomial distribution
}

model {

 for(h in 1:k) {
  for (s in (tau + 1):t){
    for (i in (s-tau + 1):s){
      target += neg_binomial_2_lpmf(obs_local[i, k] | R[s] * infectiousness[i, k], 1 / sqrt(phi));
    }
  }
 }

  R ~ gamma(1, 0.2); // Prior used by EpiEstim
  phi ~ normal(0, 1) T[0,];
}

