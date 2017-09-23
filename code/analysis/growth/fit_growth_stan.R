
extract_data <- function(df){ 
  N <- nrow(df)
  X <- df$X
  K <- ncol(X)
  Z <- df$Z 
  J <- ncol(Z)
  g <- as.numeric(factor(df$g))
  G <- length(unique(g))
  Y <- as.numeric(df$Y)
  weights <- as.numeric(X[,2])
  out <- list(N, X, K, J, Z, G, g, Y, weights)
  names(out) <- c('N', 'X', 'K', 'J', 'Z', 'G', 'g', 'Y', 'weights')
  return(out)
}

split_df <- function(df, hold){ 
  
  df_out <- split(df, df$g %in% hold)
  names(df_out) <- c('train', 'hold')
  return(df_out)
}

make_dl <- function(df){ 
  dl <- unlist( lapply( df, extract_data), recursive = F)
  names(dl) <- gsub( 'train.', '', names(dl) )
  names(dl) <- gsub( 'hold.', 'hold_', names(dl))
  return(dl)
}

fit_k_fold <- function(mydf, stan_mod, hold){ 
  
  temp_df <- split_df(mydf, hold)
  mydl <- make_dl(temp_df) 
  temp_fit <- sampling(stan_mod, data = mydl, chains = 4, thin = 1, iter = 2000, cores = 4, control = list(max_treedepth = 12, adapt_delta = 0.85))
  
  out <- c(log_lik = sum(log(colMeans(exp(extract_log_lik(temp_fit))))), 
           hold_log_lik = sum(log(colMeans(exp(extract_log_lik(temp_fit, 'hold_log_lik'))))))
  
  return(out)
}

plot_x_y <- function(myfit, X, Y, iter){
  Y_hat <- extract( myfit, 'Y_hat')$Y_hat
  
  par(mfrow = c(2,2))
  
  plot(X, Y)
  abline(0,1)
  
  plot(X, Y_hat[iter,])
  abline(0,1)
  
  plot(Y, Y_hat[iter, ])
  abline(0,1)
  
  par(mfrow = c(1,1))
}

library(rstan)
library(bayesplot)
library(loo)
library(dplyr)
library(shinystan)

rt  <- stanc('code/analysis/growth/simple_stan_lmer_censored_mvar2.stan')
stan_mod  <- stan_model(stanc_ret = rt, verbose = FALSE)

dat <- readRDS('data/temp_data/ARTR_growth_survival_dataframe.RDS') 

#### process data 
process_data <- function(x){}
dat <- subset(dat, Period == 'Historical')
dat <- dat[complete.cases(dat), ]
C <- as.matrix(data.frame( Moist1 = rowMeans(cbind(dat$C.VWC.sp.0, dat$C.VWC.su.1, dat$C.VWC.f.1, dat$C.VWC.sp.1)), 
                           Temp1 = rowMeans( cbind(dat$C.T.sp.1, dat$C.T.sp.0))))
C <- cbind(C , C[,1]*C[, 2] )
dat$C <- scale(C)
dat$W <- scale(dat$W)
dat$Group <- factor(dat$gid)

dat$sqrt_area.t0 <- sqrt(exp(dat$logarea.t0))
dat$sqrt_area.t1 <- sqrt(exp(dat$logarea.t1))

dat$Y <- scale(dat$sqrt_area.t1)
dat$X <- cbind(scale(dat$sqrt_area.t0), dat$sqrt_area.t0 == min(dat$sqrt_area.t0))

dat$X <- dat$X[,1:2]
dat$X <- model.matrix( ~ X[,1] + factor(X[,2]) + W + C, data = dat )

dat$Z <- dat$X[, 1:3]
dat$g <- factor(dat$yid)
dat <- split_df(dat, 2)
dl <- make_dl(dat)

# account for left censored data 
dl$obs <- which(dl$Y > min(dl$Y))
dl$cens <- which(dl$Y <=min(dl$Y))
dl$N_obs <- length(dl$obs)
dl$N_cens <- length(dl$cens)
dl$U <- min(dl$Y)
dl$Y_obs <- dl$Y[dl$obs]

myfit <- sampling(stan_mod, data = dl, chains = 4, thin = 1, iter = 2000, cores = 4, control = list(max_treedepth = 12, adapt_delta = 0.85), seed = 1)

Y <- dl$Y
Y_obs <- dl$Y_obs
X <- dl$X[, 2] 

plot_x_y(myfit, X, Y, iter = 100)

launch_shinystan(myfit)

L_u <- matrix( summary(myfit, 'L_u')$summary[,6], dl$J,dl$J, byrow = T)
L_u%*%t(L_u)

Sigma_L <- matrix( summary(myfit, 'Sigma_L')$summary[,6], dl$J,dl$J, byrow = T)
Sigma_L%*%t(Sigma_L)


# Plot marginal climate effects 
# fit_beta <- summary(myfit, 'beta')$summary[, '50%']
# TempX <- data.frame(dl$X)
# TempX[,2] <- sort(TempX[,2])
# TempX[,3] <- sort(TempX[,3])
# TempX[,4:7] <- 0
# TempX[, 9:10] <- 0
# TempX <- rbind( TempX , TempX , TempX )
# 
# TempX[ , 'CMoist1'] <- sort( rep (quantile(dl$X[, 'CMoist1'], c(0.025, 0.5, 0.975)), nrow(dl$X)))
# Temp_mu <- as.matrix( TempX) %*% fit_beta
# 
# test <- data.frame(y= Temp_mu , x = TempX[,1], group = as.numeric(factor(TempX[, 'CMoist1'])))
# test <- test %>% arrange( group ,  x ) 
# 
# test_dat <- data.frame(x = dl$X[, 2], y = dl$Y, group = cut(dl$X[, 'CMoist1'], quantile(dl$X[, 'CMoist1'], c(0, 0.25, 0.75, 1)), include.lowest = T))
# test_dat$group  <- as.numeric(factor(test_dat$group))
# 
# load('figures/my_plotting_theme.Rdata')
# ggplot(test_dat, aes( x = x , y =y , color = factor(group), alpha = factor(group))) + geom_point() + 
#   geom_line( data = test,  aes( x = x , y = y), alpha = 1) + 
#   scale_color_manual(values = my_colors[c(3,2,4)]) + 
#   scale_alpha_manual(values = c(0.5, 0.1, 0.5))
# 
