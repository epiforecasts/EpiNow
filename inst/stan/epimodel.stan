
data {
  int t; // number of time steps
  int <lower = 0> obs_imported[t]; // imported cases
  int <lower = 0> obs_local[t]; // local cases
  int tau; // length of window
  real <lower = 0> si_loc;
  real <lower = 0> si_scale;
  real <lower = 0> inc_loc;
  real <lower = 0> inc_scale;
  real <lower = 0> delay_alpha;
  real <lower = 0> delay_beta;
}

transformed data {
  
  // all vectors and matrices length / ncol = 2t - 1
  // this corresponds to probability of the distribution being
  // equal to -t, through to 0 at index t, to t
  int indw; // index variable
  vector[(2 * t) - 1] sivec; // vector of discretised serial interval
  matrix[(2 * t) - 1, (2 * t) - 1] simat; // serial interval matrix (for convolution purposes)
  vector[(2 * t) - 1] incvec; // vector of discretised incubation period
  matrix[(2 * t) - 1, (2 * t) - 1] incmat; // incubation period matrix (convolutions)
  matrix[(2 * t) - 1, (2 * t) - 1] incmat2; // incubation period matrix (different convolutions)
  vector[(2 * t) - 1] conv_inc; // discretised distribution of difference between two incubation periods
  vector[(2 * t) - 1] wv; // convolution of vector above and the serial interval
  matrix[(2 * t) - 1, (2 * t) - 1] delaymat; // convolution matrix for delay distribution
  vector[(2 * t) - 1] delayvec; // vector of discretised delay distribution
  vector[(2 * t) - 1] conv_delay; // discretised distribution of difference between two delays
  matrix[(2 * t) - 1, (2 * t) - 1] conv_delay_mat; // convolution matrix
  vector[(2 * t) - 1] ci; // final product, confirmation interval (can be negative)
  vector[t] infectiousness; // infectiousness 
  vector[2 * t] upscaled_cases; // upscaled confirmed cases
  vector[(2 * t) - 1] inc_conv_delay; // convolution of 1 incubation period + 1 delay
  vector[t] inf_back; // infectiousness by infection date
  
  // Discretised serial interval distribution and incubation period
  for(i in 1:((2 * t) - 1)){
    sivec[i] = (i < t) ? 0 : lognormal_cdf(i - t + 1, si_loc, si_scale) - lognormal_cdf(i - t, si_loc, si_scale);
    incvec[i] = (i < t) ? 0 : lognormal_cdf(i - t + 1, inc_loc, inc_scale) - lognormal_cdf(i - t, inc_loc, inc_scale);
  }
  
  // Matrices for later convolution calculations
  // these are constructed so that I can do matrix * vector of distribution
  // and get the convolution I want
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
  
  
  // Discretised delay distribution
  for(i in 1:((2 * t) - 1)) {
        delayvec[i] = (i >= t) ? gamma_cdf(i - t + 1, delay_alpha, 1 / delay_beta) - gamma_cdf(i - t , delay_alpha, 1 / delay_beta) : 0;
  }

  // calculate matrix for convolution
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      delaymat[i, j] = ((t - i + j) <= 0) ? 0: (((t - i + j) >= ((2 * t) -1)) ? 0 : delayvec[(t - i + j)]);
    }
  }
  
  // Difference of two confirmation delays
  conv_delay = delaymat * delayvec;
  
  // Matrix for convolution with serial interval
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      conv_delay_mat[i, j] = ((t + i - j) <= 0) ? 0: (((t + i - j) >= ((2 * t) -1)) ? 0 : conv_delay[(t + i - j)]);
    }
  }
  
  // 1 incubation period + 1 delay
  inc_conv_delay = incmat2 * delayvec;
  
  // Confirmation interval
  ci = conv_delay_mat * wv;
  
  // Upscaling confirmed cases 
  for(g in 1:t) {
    upscaled_cases[g] = (obs_imported[g] + obs_local[g]) / sum(ci[(t - (t - g)):(2 * t - 1)]);
  }
  
  // Calculate infectiousness
  for (s in 1:t) {
    infectiousness[s] = 0;
    for(i in 1:t) {
      infectiousness[s] += upscaled_cases[i] * ci[(s - i + t)];
    }
  }
  
  // Infectiousness by infection date
  for(i in 1:t){
    inf_back[i] = 0;
  }
  
  for(k in 1:t) {
    for(j in 1:t) {
      inf_back[k] += infectiousness[j] * inc_conv_delay[j - k + t];
    }
  }
}

parameters{
  real <lower = 0> R[t]; // Effective reproduction number over time
  real <lower = 0> phi; // Dispersion of negative binomial distribution
}

model {

  // Priors for Rts and negative binomial dispersion
  R ~ gamma(1, 0.2);
  phi ~ normal(0, 1) T[0,];
  
  // Log likelihood of observed local cases given infectiousness and Rts
  for (s in (tau + 1):t){
    for (i in (s-tau + 1):s){
      target += neg_binomial_2_lpmf(obs_local[i] | R[s] * infectiousness[i], 1 / sqrt(phi));
    }
  }
  
}

generated quantities {
  vector[t-7] inf_cases;
  vector[t] inf_R;

  for(i in 1:t){
    inf_R[i] = 0;
  }

  for(k in 1:t) {
    for(j in 1:t) {
      inf_R[k] += R[j] * inc_conv_delay[j - k + t];
    }
  }

  for(i in 1:(t-7)) {
    inf_cases[i] = neg_binomial_2_rng(inf_R[i] * inf_back[i],  1 / sqrt(phi));
  }

}
