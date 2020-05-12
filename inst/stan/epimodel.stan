data {
  int t; // number of time steps
  int k; // number of serial interval and delay samples
  int n; // length of serial interval distributions
  int <lower = 0> obs_local[t, k]; // local cases
  real <lower = 0> infectiousness[t, k]; // back-calculated infectiousness
  int tau; // length of window
  matrix[n, k] w; // matrix of different serial interval distributions
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

