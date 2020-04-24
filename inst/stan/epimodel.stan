
data {
  int t; // number of time steps
  int <lower = 0> obs_imported[t]; // imported cases
  int <lower = 0> obs_local[t]; // local cases
  int tau; // length of window
  real <lower = 0> si_loc;
  real <lower = 0> si_scale;
  real <lower = 0> inc_loc;
  real <lower = 0> inc_scale;
  int N; // number of delay samples
  vector[N] low; // lower bound for delay
  vector[N] up; // upper bound for delay
}

transformed data {
  int indw;
  vector[(2 * t) - 1] sivec;
  matrix[(2 * t) - 1, (2 * t) - 1] simat;
  vector[(2 * t) - 1] incvec;
  matrix[(2 * t) - 1, (2 * t) - 1] incmat;
  matrix[(2 * t) - 1, (2 * t) - 1] incmat2;
  vector[(2 * t) - 1] conv_inc;
  vector[(2 * t) - 1] wv;
  
  // Discretised serial interval distribution and incubation period
  for(i in 1:((2 * t) - 1)){
    sivec[i] = (i < t) ? 0 : lognormal_cdf(i - t + 1, si_loc, si_scale) - lognormal_cdf(i - t, si_loc, si_scale);
    incvec[i] = (i < t) ? 0 : lognormal_cdf(i - t + 1, inc_loc, inc_scale) - lognormal_cdf(i - t, inc_loc, inc_scale);
  }
  
  // Matrices for later convolution calculations
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      indw = t + i - j;
      simat[i, j] = (indw <= 0) ? 0: ((indw >= ((2 * t) -1)) ? 0 : sivec[indw]);
      incmat[i, j] = ((t - i + j) <= 0) ? 0: (((t - i + j) >= ((2 * t) -1)) ? 0 : incvec[(t - i + j)]);
      incmat2[i, j] = (indw <= 0) ? 0: ((indw >= ((2 * t) -1)) ? 0 : incvec[indw]);
    }
  }
  
  // Difference of two incubation periods
  conv_inc = incmat * incvec;
  
  // Convolution of incubation period convolution and serial interval
  wv = simat * conv_inc;
}

parameters{
  real <lower = 0> R[t]; // Effective reproduction number over time
  real <lower = 0> phi; // Dispersion of negative binomial distribution
  real<lower = 0> alpha;
  real<lower = 0> beta;
}

transformed parameters {
  matrix[(2 * t) - 1, (2 * t) - 1] delaymat;
  vector[(2 * t) - 1] delayvec;
  vector[(2 * t) - 1] conv_delay;
  matrix[(2 * t) - 1, (2 * t) - 1] conv_delay_mat;
  vector[(2 * t) - 1] ci;
  real infectiousness[t];
  
  // Discretised delay distribution
  for(i in 1:((2 * t) - 1)) {
        delayvec[i] = (i >= t) ? gamma_cdf(i - t + 1, alpha, beta) - gamma_cdf(i - t , alpha, beta) : 0;
  }

  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      delaymat[i, j] = ((t - i + j) <= 0) ? 0: (((t - i + j) >= ((2 * t) -1)) ? 0 : delayvec[(t - i + j)]);
    }
  }
  
  // Difference of two confirmation delays
  conv_delay = delaymat * delayvec;
  
  
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      conv_delay_mat[i, j] = ((t + i - j) <= 0) ? 0: (((t + i - j) >= ((2 * t) -1)) ? 0 : conv_delay[(t + i - j)]);
    }
  }
  
  // Confirmation interval
  ci = conv_delay_mat * wv;
  
  // Calculate infectiousness

  for (s in 1:t) {
    infectiousness[s] = 0;
    for(i in 1:t) {
      infectiousness[s] += (obs_imported[i] + obs_local[i]) * ci[(s - i + t)];
    }
  }
}

model {
  // Delay distribution likelihood
  
  alpha ~ normal(0, 1) T[0,];
  beta ~ normal(0, 1) T[0,];
  // 

  for(i in 1:N){
    target += log(gamma_cdf(up[i] , alpha, beta) - gamma_cdf(low[i] , alpha, beta));
  }
  
  for (s in (tau + 1):t){
    for (i in (s-tau + 1):s){
      target += neg_binomial_2_lpmf(obs_local[i] | R[s] * infectiousness[i], 1 / sqrt(phi));
    }
  }

  R ~ gamma(1, 0.2);
  phi ~ normal(0, 1) T[0,];
  
}

generated quantities {
  // vector[t] onset_R;
  vector[t] actual_R;
  vector[t] inf_back;
  vector[t-7] inf_cases;
  vector[(2 * t) - 1] inc_conv_delay;

  inc_conv_delay = incmat2 * delayvec;
  
  for(i in 1:t){
    actual_R[i] = 0;
    inf_back[i] = 0;
  }

  for(k in 1:t) {
    for(j in 1:t) {
      actual_R[k] += R[j] * inc_conv_delay[j - k + t];
      inf_back[k] += infectiousness[j] * inc_conv_delay[j - k + t];
    }
  }
  
  for(i in 1:(t-7)) {
    inf_cases[i] = neg_binomial_2_rng(actual_R[i] * inf_back[i],  1 / sqrt(phi));
  }

}
