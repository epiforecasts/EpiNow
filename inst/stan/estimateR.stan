functions {
  //Spline adapted from here: 
  //https://github.com/milkha/Splines_in_Stan/blob/master/b_spline_penalized.stan
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t))
        b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]); 
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) / 
             (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) / 
                 (ext_knots[ind+order] - ext_knots[ind+1]);
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) + 
                 w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}


data {
  int t; // number of time steps
  int k; // number of interval and case samples
  int n; // length of interval distributions
  int <lower = 0> obs_imported[t, k]; // imported cases
  int <lower = 0> obs_local[t, k]; // local cases
  real times[t]; //Time steps
  real intervals[n, k]; // matrix of different interval distributions
  real <lower = 0> r_mean;
  real <lower = 0> r_sd;
  int num_knots; //Starting number of knots for the spline
  vector[num_knots] knots; //Knots vector
  int spline_degree; //Degree of the spline
}

transformed data{
    // Set up transformed data objects
  vector[t] infectiousness[k];
  real r_alpha; //alpha parameter of the R gamma prior
  real r_beta;  //beta parameter of the R gamma prior
  
    //Set up the spline
  int num_basis = num_knots + spline_degree - 1; 
  matrix[num_basis, t] B;
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots;
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (ind in 1:num_basis)
    B[ind,:] = to_row_vector(build_b_spline(times, to_array_1d(ext_knots), ind, spline_degree + 1));
  B[num_knots + spline_degree - 1, t] = 1;
  

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
    
     //If infectiousness is ever zero set to be nearly zero to avoid sampling issues
     if (infectiousness[j][s] == 0) {
       infectiousness[j][s] = 0.0000001;
      }
    }
  }
}

parameters{
  real <lower = 0> phi; // Dispersion of negative binomial distribution
  
  //Parameterise spline
  row_vector<lower = 0>[num_basis] init_R;
  real<lower=0> tau;
}

transformed parameters {
  row_vector[num_basis] spline_R;
  vector<lower = 0>[t] R;
  spline_R[1] = init_R[1];
  for (i in 2:num_basis) {
        spline_R[i] = spline_R[i-1] + init_R[i]*tau; 
  }
  R = to_vector(spline_R * B);
}

model {

  init_R ~ gamma(r_alpha, r_beta); // Prior  on Rt
  phi ~ exponential(1); //Prior on Phi
  tau ~ normal(0, 1); //Smoothing prior on the spline
  
 //Build likelihood across all samples
 for (s in 2:t){
      for(j in 1:k) {
          real mean_cases = R[s] * infectiousness[j][s];
          target += neg_binomial_2_lpmf(obs_local[s, j] | mean_cases, phi);
          }
      }
}

