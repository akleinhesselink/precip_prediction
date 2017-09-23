data{
  // training datalist, historical observations 
  int<lower=0> N;         // observations
  int<lower=0> N_cens;    // censored 
  int<lower=0> N_obs;     // not censored 
  int<lower=0, upper=N> obs[N_obs];  // index of not censored 
  int<lower=0, upper=N> cens[N_cens]; // index of censored observations 
  int<lower=0> J;         // number of group level effects
  int<lower=0> K;         // number of covariates in the design matrix 
  vector[N_obs] Y_obs;    // only obs with valid size
  vector[N] Y;            // all observations
  row_vector[J] Z[N];     // simple intercept x size design matrix 
  int<lower=0> G;         // groups
  int<lower=0> g[N];      // group id
  matrix[N,K] X;          // covariate matrix   
  vector[N] weights;
  
  real<upper=min(Y_obs)> U;  // Upper limit of censored data 

  // holdout data
  int<lower=0> hold_N;         // observations
  vector[hold_N] hold_Y;            // observation vector
  row_vector[J] hold_Z[hold_N];     // simple intercept x size design matrix
  int<lower=0> hold_G;         // groups
  int<lower=0> hold_g[hold_N];      // group id
  matrix[hold_N,K] hold_X;          // covariate matrix
  vector[hold_N] hold_weights;      // plant size scales sigma 
  
}
transformed data{ 
  vector[J] alpha;               // prior on dirichlet distribution
  
  for(i in 1:J)
    alpha[i] = 1; 
}
parameters{
  // for training data model  
	vector[K] beta;                // fixed effects
  cholesky_factor_corr[J] L_u;    // cholesky factor for correlation for intcpt/slope of year effects
	matrix[J,G] u_raw;              // raw group effects 
	simplex[J] pi_;                 // pi simplex for diagonal of the covariance matrix 
	real<lower=0> tau;              // scale parameter for covariance matrix
	real eta;                       // intercept variance scale
	real etaSize;                   // slope of variance scale 
}
transformed parameters{
  real mu[N];             
  real<lower=0> sigma[N];
  
  vector[J] u[G];                 // scaled and correlated group effects 
  matrix[J,J] Sigma_L;            // cholesky of covariance matrix
  vector[N_obs] mu_obs;
  vector[N_cens] mu_cens;

  vector<lower=0>[N_obs] sigma_obs;
  vector<lower=0>[N_cens] sigma_cens;
  
  {
    vector[N] fixef;              // fixed effects 
    vector[J] sigma_j;   
    sigma_j = pi_*J*tau^2;        // generates covMat diagonal; Explained by rstanarm glmer vignette
    Sigma_L = diag_pre_multiply(sigma_j, L_u);  // multiply variance by correlation  
  
    for(j in 1:G)
      u[j] = Sigma_L * col(u_raw, j);    

    fixef = X*beta;
    for(n in 1:N){
      mu[n] = fixef[n] + Z[n]*u[g[n]];
      sigma[n] = fmax(sqrt(exp(eta + etaSize*weights[n])), 0.01);
    }
  }
  
  // split out the observed and censored predictors 
  for(i in 1:N_obs){
    mu_obs[i] = mu[obs[i]];
    sigma_obs[i] = sigma[obs[i]];
  }
  
  for(i in 1:N_cens){
    mu_cens[i] = mu[cens[i]];  
    sigma_cens[i] = sigma[cens[i]];
  }
}
model{
  // Priors
  beta ~ normal(0,5);
  
  etaSize ~ normal(0,1);
  eta ~ normal(0,1);

  pi_ ~ dirichlet(alpha);       // dirichlet as per rstanarm glmer vignette
  tau ~ gamma(1,1);             // gamma as per rstanarm glmer vignette
  
  L_u ~ lkj_corr_cholesky(1.0);

  to_vector(u_raw) ~ normal(0,1);
		
  // Likelihood
  Y_obs ~ normal(mu_obs, sigma_obs);
  target += normal_lcdf(U | mu_cens, sigma_cens);  // integrate out the censored observations 

}
generated quantities {
  vector[N] Y_hat;
  vector[N_obs] Y_hat_obs;
  vector[N] log_lik;
  
  vector[hold_N] hold_mu;                    // linear predictor
  vector[hold_N] hold_sigma;                    // linear predictor
  vector[J] hold_u[hold_G];                  // scaled and correlated group effects
  vector[hold_N] hold_log_lik;
  vector[hold_N] hold_fixef;                // fixed effects
  matrix[J, hold_G] hold_u_raw;              // raw group effects
  
  for(i in 1:N){
    Y_hat[i] = normal_rng(mu[i], sigma[i]);
    log_lik[i] = normal_lpdf(Y[i] | mu[i], sigma[i]);
  }
  
  for(i in 1:N_obs)
    Y_hat_obs[i] = normal_rng(mu_obs[i], sigma_obs[i]);
    
  for(i in 1:hold_G)
    for(j in 1:J)
      hold_u_raw[j, i] = normal_rng(0,1);

  for(j in 1:hold_G)
    hold_u[j] = Sigma_L * col(hold_u_raw, j);


  hold_fixef = hold_X*beta;

  for(i in 1:hold_N){
    hold_mu[i] = hold_fixef[i] + hold_Z[i]*hold_u[hold_g[i]];
    hold_sigma[i] = fmax(sqrt(exp(eta + etaSize*weights[i])), 0.01);
  }
  
  for(i in 1:hold_N)
    hold_log_lik[i] = normal_lpdf(hold_Y[i] | hold_mu[i], hold_sigma[i]);

}
