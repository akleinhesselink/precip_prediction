data{
  // training datalist, historical observations 
  int<lower=0> N;                       // # observations total 
  vector<lower=0>[N] Y;                 // untransformed response
  int<lower=0> K;                       // # of fixed effects
  matrix[N,K] X;                        // fixed effects matrix   
  int<lower=0> J;                       // # of group level effects
  row_vector[J] Z[N];                   // group level effects matrix
  int<lower=0> G;                       // groups
  int<lower=0> g[N];                    // group id
  int<lower=0> D;                       // # effects in dispersion model 
  matrix[N,D] E;                          // dispersion model matrix 
  int<lower=0, upper=N> N_cens;         // # censored 
  int<lower=0, upper=N> N_obs;          // # observed 
  int<lower=0, upper=N> obs[N_obs];     // observed index
  int<lower=0, upper=N> cens[N_cens];   // censored index 
  int<lower=0, upper=1 > censored[N];   // flag for censored data 
  real<lower=0, upper=max(Y)> U;        // upper limit of left censored data
  vector<lower=U>[N_obs] Y_obs;         // non-censored observations 
  
  // holdout data
  int<lower=0> hold_N;                  // hold_N == 2 if no held out data   
  vector[hold_N] hold_Y;
  matrix[hold_N,K] hold_X;            
  row_vector[J] hold_Z[hold_N];       
  int<lower=0> hold_G;                
  int<lower=0> hold_g[hold_N];        
  matrix[hold_N, D] hold_E;  
}
transformed data{ 
  vector[J] a;                          // prior on dirichlet distribution
  real Y_min;                           // roundup Y predictions to Y_min 
  
  for(i in 1:J)
    a[i] = 1; 
    
  Y_min = min(Y);
}
parameters{
  // for training data model  
	vector[K] theta;                      // fixed effects
  vector<lower=0>[D-1] phi_scale;         // Dispersion model parameters
	real phi_exp; 
	simplex[J] pi_;                       // simplex for diagonal of group-level covariance matrix 
	real<lower=0> tau;                    // scale parameter for group-level covariance matrix
  cholesky_factor_corr[J] L_u;          // cholesky factor for group-level correlation
	matrix[J,G] u_raw;                    // raw group-level effects 

	//vector<lower=0, upper= U>[N_cens] Y_cens; 
}
transformed parameters{
  // for training data model  
  vector<lower=0>[N] mu;                // linear predictor 
  vector[N] alpha;                      // shape parameter for the gamma distribution
  vector[N] beta;                       // scale parameter for the gamma distribution
  vector<lower=0>[N] phi;                        // Dispersion 
  vector[J] u[G];                       // group-level effects 
  matrix[J,J] Sigma_L;                  // cholesky of covariance matrix
  vector[J] sigma_j;                    // diagonal of covariance matrix 
  vector<lower=0>[N_obs] alpha_obs;     
  vector<lower=0>[N_obs] beta_obs;      
  vector<lower=0>[N_cens] alpha_cens;   
  vector<lower=0>[N_cens] beta_cens;    
    
    
  sigma_j = pi_*J*tau^2;                      // Explained by rstanarm glmer vignette
  Sigma_L = diag_pre_multiply(sigma_j, L_u);  // multiply variance by correlation  
  
  for(j in 1:G)
    u[j] = Sigma_L * col(u_raw, j);    
  
  {
    vector[N] fixef;              
    fixef = X*theta;
    for(i in 1:N){
      mu[i] = exp(fixef[i] + Z[i]*u[g[i]]);
      phi[i] = (E[i,1:2]*phi_scale)*pow(E[i,3], phi_exp);
    }    
  }
  
  alpha = mu .* mu ./ phi; 
  beta = mu ./ phi;
  
  // split out the observed and censored predictors 
  for(i in 1:N_obs){
    alpha_obs[i] = alpha[obs[i]];
    beta_obs[i] = beta[obs[i]];
  }
  
  for(i in 1:N_cens){
    alpha_cens[i] = alpha[cens[i]];  
    beta_cens[i] = beta[cens[i]];  
  }
}
model{
  // Priors
  theta[1] ~ cauchy(0,5);
  theta[2:K] ~ normal(0,5);
  phi_scale[1] ~ normal(1,1);
  phi_scale[2] ~ normal(0,1);
  phi_exp ~ normal(0,1);
  pi_ ~ dirichlet(a);                   // dirichlet as per rstanarm glmer vignette
  tau ~ gamma(1,1);                     // gamma as per rstanarm glmer vignette
  L_u ~ lkj_corr_cholesky(1.0);
  to_vector(u_raw) ~ normal(0,1);
  //Y_cens ~ normal(0,1); 
  
  // Likelihood
  Y_obs ~ gamma(alpha_obs, beta_obs);
  target += gamma_lcdf(U | alpha_cens, beta_cens);
  //Y_cens ~ gamma(alpha_cens, beta_cens);
}
generated quantities {
  vector<lower=0>[N] Y_hat;
  vector[N] log_Y_hat;
  vector[N] log_lik;

  for(i in 1:N){
    Y_hat[i] = fmax(gamma_rng(alpha[i], beta[i]), Y_min);
    log_Y_hat[i] = log(Y_hat[i]);
    
    if(censored[i] == 0){ 
      log_lik[i] = gamma_lpdf(Y[i] | alpha[i], beta[i]);
    }else if(censored[i] == 1){ 
      log_lik[i] = gamma_lcdf(U | alpha[i], beta[i]);
    }
  }
  
  if(hold_N > 2){
    // vector[hold_N] hold_mu;                    // linear predictor
    // vector[hold_N] hold_sigma;                    // linear predictor
    // vector[J] hold_u[hold_G];                  // scaled and correlated group effects
    // vector[hold_N] hold_log_lik;
    // vector[hold_N] hold_fixef;                // fixed effects
    // matrix[J, hold_G] hold_u_raw;              // raw group effects
      
  }
  
  // for(i in 1:N_obs)
  //   Y_hat_obs[i] = fmax(gamma_rng(alpha_obs[i], beta_obs[i]), U);
    
  // for(i in 1:hold_G)
  //   for(j in 1:J)
  //     hold_u_raw[j, i] = normal_rng(0,1);
  // 
  // for(j in 1:hold_G)
  //   hold_u[j] = Sigma_L * col(hold_u_raw, j);
  // 
  // 
  // hold_fixef = hold_X*beta;
  // 
  // for(i in 1:hold_N){
  //   hold_mu[i] = hold_fixef[i] + hold_Z[i]*hold_u[hold_g[i]];
  //   hold_sigma[i] = eta*inv_logit(etaSize*hold_weights[i]);
  // }
  // 
  // for(i in 1:hold_N)
  //   hold_log_lik[i] = normal_lpdf(hold_Y[i] | hold_mu[i], hold_sigma[i]);

}
