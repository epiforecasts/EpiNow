
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
  int indw; // index variables
  int indv; 
  int indc;
  vector[(2 * t) - 1] sivec; // vector of discretised serial interval
  matrix[(2 * t) - 1, (2 * t) - 1] simat; // serial interval matrix (for convolution purposes)
  vector[(2 * t) - 1] incvec; // vector of discretised incubation period
  matrix[(2 * t) - 1, (2 * t) - 1] incmat; // incubation period matrix (different convolutions)
  matrix[(2 * t) - 1, (2 * t) - 1] delaymat; // convolution matrix for delay distribution
  vector[(2 * t) - 1] delayvec; // vector of discretised delay distribution
  vector[(2 * t) - 1] conv_delay; // discretised distribution of difference between two delays
  matrix[(2 * t) - 1, (2 * t) - 1] conv_delay_mat; // convolution matrix
  vector[(2 * t) - 1] ci; // final product, confirmation interval (can be negative)
  vector[(2 * t) - 1] infectiousness; // infectiousness 
  // vector[2 * t] upscaled_cases; // upscaled confirmed cases
  vector[(2 * t) - 1] inc_conv_delay; // convolution of 1 incubation period + 1 delay
  vector[(2 * t) - 1] inf_back; // infectiousness by infection date
  matrix[(2 * t) - 1, (2 * t) - 1] cimat;
  int obs_app[(2 * t) - 1];
  

  for(i in 1:((2 * t) - 1)){
    
    // Discretised Serial interval distribution
    sivec[i] = (i < t) ? 0 : lognormal_cdf(i - t + 1, si_loc, si_scale) - 
    lognormal_cdf(i - t, si_loc, si_scale);
    
    // Discretised Incubation Period distribution
    incvec[i] = (i < t) ? 0 : lognormal_cdf(i - t + 1, inc_loc, inc_scale) - 
    lognormal_cdf(i - t, inc_loc, inc_scale);
    
    // Discretised Notification Delay distribution
    delayvec[i] = (i >= t) ? gamma_cdf(i - t + 1, delay_alpha, delay_beta) - 
    gamma_cdf(i - t , delay_alpha, delay_beta) : 0;
  }
  
  // Matrices for later convolution calculations
  // these are constructed so that I can do matrix * vector of distribution
  // and get the convolution I want
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      indw = t + i - j;
      // Matrix for Serial Interval
      simat[i, j] = (indw <= 0) ? 0: ((indw >= ((2 * t) -1)) ? 0 : sivec[indw]);
      // Matrix for Incubation Period
      incmat[i, j] = (indw <= 0) ? 0: ((indw >= ((2 * t) -1)) ? 0 : incvec[indw]);
      // Matrix for Notification Delay, indices are different because we want the difference
      delaymat[i, j] = ((t - i + j) <= 0) ? 0: (((t - i + j) >= ((2 * t) -1)) ? 0 : delayvec[(t - i + j)]);
    }
  }
  
  // Difference of two Notification Delays
  conv_delay = delaymat * delayvec;
  
  // Matrix for convolution with serial interval
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      indv = t + i - j;
      conv_delay_mat[i, j] = (indv <= 0) ? 0: ((indv >= ((2 * t) -1)) ? 0 : conv_delay[indv]);
    }
  }
  
  // Confirmation interval = Serial Interval + (Notification Delay convolution) convolution
  ci = simat * conv_delay;
  
  // Incubation Period + Notification Delay convolution
  inc_conv_delay = incmat * delayvec;
  
  for(i in 1:((2 * t) - 1)) {
    obs_app[i] = i > t ? 0 : obs_local[i] + obs_imported[i];
  }
  
  for(i in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      // Matrix for Confirmation Interval
      cimat[i, j] = ((t + i - j) <= 0) ? 0: (((t + i - j) >= ((2 * t) -1)) ? 0 : ci[(t + i - j)]);
    }
  }
      
  infectiousness = cimat * to_vector(obs_app);
  
  for(i in 1:((2 * t) - 1)) {
    infectiousness[i] = max({infectiousness[i], 1E-06});
  }
  
  // // Upscaling confirmed cases 
  // for(g in 1:t) {
  //   upscaled_cases[g] = (obs_imported[g] + obs_local[g]) / sum(ci[(t - (t - g)):(2 * t - 1)]);
  // }
  // 
  // // Calculate infectiousness
  // for (s in 1:t) {
  //   infectiousness[s] = 0;
  //   for(i in 1:t) {
  //     infectiousness[s] += upscaled_cases[i] * ci[(s - i + t)];
  //   }
  // }
  
  // Infectiousness by infection date
  for(i in 1:((2 * t) - 1)){
    inf_back[i] = 0;
  }

  for(k in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      indc = j - k + t;
      inf_back[k] += (indc <= 0) ? 0 : indc >= ((2 * t) - 1) ? 0 : infectiousness[j] * inc_conv_delay[j - k + t];
      // inf_back[k] += infectiousness[j] * inc_conv_delay[j - k + t];
    }
  }
}

parameters{
  real <lower = 0> R[(2 * t) - 1]; // Effective reproduction number over time
  real <lower = 0> phi; // Dispersion of negative binomial distribution
}

model {

  // Priors for Rts and negative binomial dispersion
  R ~ gamma(1, 0.2);
  phi ~ normal(0, 1) T[0,];
  
  // Log likelihood of observed local cases given infectiousness and Rts
  for (s in (tau + 1):((2 * t) - 1)){
    for (i in (s-tau + 1):s){
      target += neg_binomial_2_lpmf(obs_app[i] | R[s] * infectiousness[i], 1 / sqrt(phi));
    }
  }
  
}

generated quantities {
  vector[(2 * t) - 1] inf_cases;
  vector[(2 * t) - 1] inf_R;
  vector[(2 * t) - 1] confirmation_interval;
  vector[(2 * t) - 1] notification_delay;
  vector[(2 * t) - 1] serial_interval;
  vector[(2 * t) - 1] notif_incub_conv;
  vector[(2 * t) - 1] incubation_period;
  vector[(2 * t) - 1] delay_conv;
  vector[(2 * t) - 1] infectiousness_out;
  vector[(2 * t) - 1] inf_back_out;
  int obs_app_out[(2 * t) - 1];

  confirmation_interval = ci;
  notification_delay = delayvec;
  serial_interval = sivec;
  notif_incub_conv = inc_conv_delay;
  incubation_period = incvec;
  infectiousness_out = infectiousness;
  inf_back_out = inf_back;
  delay_conv = conv_delay;
  obs_app_out = obs_app;


  for(i in 1:((2 * t) - 1)){
    inf_R[i] = 0;
  }

  for(k in 1:((2 * t) - 1)) {
    for(j in 1:((2 * t) - 1)) {
      inf_R[k] += (j - k + t <= 0) ? 0 : j - k + t >= ((2 * t) - 1) ? 0 : R[j] * inc_conv_delay[j - k + t];
    }
  }

  for(i in 1:((2 * t) - 1)) {
    inf_cases[i] = neg_binomial_2_rng(max({inf_R[i] * inf_back[i],0.00001}),  1 / sqrt(phi));
  }

}
