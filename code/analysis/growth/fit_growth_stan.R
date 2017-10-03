extract_data <- function(df){ 
  N <- nrow(df)
  X <- df$X
  K <- ncol(X)
  Z <- df$Z 
  J <- ncol(Z)
  g <- as.numeric(factor(df$g))
  G <- length(unique(g))
  Y <- as.numeric(df$Y)
  E <- df$E
  D <- ncol(E) 
  
  rm(df)
  out <- lapply( ls(), function(x) eval(parse(text = x)))
  names(out) <- ls()[!ls() == 'out']

  return(out)
}

split_df <- function(df, hold){ 
  if(all(hold == 0)){
    df_out <- split(df, df$g %in% hold)
    df_out$True <- data.frame(Y = rep(0,2))
    df_out$True$X = matrix(0, ncol = ncol(df$X), nrow = 2)
    df_out$True$Z = matrix(0, ncol = ncol(df$Z), nrow = 2)
    df_out$True$g = rep(0,2)
    df_out$True$E = matrix(0, ncol = ncol(df$E), nrow = 2)
    
  }else if(any(hold > 0)){ 
    df_out <- split(df, df$g %in% hold)
  }
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

plot_x_y <- function(myfit, X, Y, iter, bt = F){
  
  if(bt){
    Y_hat <- extract( myfit, 'Y_hat_bt')$Y_hat_bt
  }else if(!bt){
    Y_hat <- extract( myfit, 'Y_hat')$Y_hat
  }
  
  ylims = c(min(c(Y_hat[iter,], Y)), max(c(Y_hat[iter, ], Y)))
  
  par(mfrow = c(2,2))
  
  plot(X, Y, ylim = ylims)
  abline(0,1)
  
  plot(X, Y_hat[iter,], ylim = ylims)
  abline(0,1)
  
  plot(Y, Y_hat[iter, ], ylim = ylims)
  abline(0,1)
  
  par(mfrow = c(1,1))
}

process_data <- function(dat, small = 0, formX = as.formula(~ size + small + Group + W + C), formZ = as.formula(~ size + small), formE = as.formula(~ size_raw + small), center = T, ... ){
  dat <- subset(dat, Period == 'Historical')
  dat <- dat[complete.cases(dat), ]
  C <- as.matrix(data.frame( Moist1 = rowMeans(cbind(dat$C.VWC.sp.0, dat$C.VWC.su.1, dat$C.VWC.f.1, dat$C.VWC.sp.1)), 
                             Temp1 = rowMeans( cbind(dat$C.T.sp.1, dat$C.T.sp.0))))
  C <- cbind(C , C[,1]*C[, 2] )
  dat$C <- scale(C)
  dat$W <- scale(dat$W)
  dat$Group <- factor(dat$gid)
  
  dat$Y <- exp(dat$logarea.t1)
  
  dat$size <- scale(dat$logarea.t0)
  dat$small <- factor(dat$logarea.t0 < small)
  dat$size_2 <- scale(dat$logarea.t0^2)
  dat$size_raw <- exp(dat$logarea.t0)
  dat$size_min_scale <- exp(dat$size)/min(exp(dat$size))
  
  dat$X <- model.matrix(formX, data = dat)
  dat$Z <- model.matrix(formZ, data = dat)
  dat$E <- model.matrix(formE, data = dat) 

  dat$g <- factor(dat$yid)
  
  dat <- split_df(dat, ... )
  dl <- make_dl(dat)
  
  return(dl)
} 

left_censor <- function(dl, U = min(dl$Y)){ 
  # account for left censored data 
  
  dl$U <- U
  dl$obs <- which(dl$Y > U)
  dl$cens <- which(dl$Y <= U)
  dl$N_obs <- length(dl$obs)
  dl$N_cens <- length(dl$cens)
  dl$Y_obs <- dl$Y[dl$obs]
  dl$censored <- as.numeric(dl$Y <= U)
  return(dl)
}

library(rstan)
library(bayesplot)
library(loo)
library(dplyr)
library(ggplot2)

dat <- readRDS('~/Dropbox/projects/precip_prediction/data/temp_data/POSE_growth_survival_dataframe.RDS')
dl <- process_data(dat, small = -1, formX = as.formula(~size + small), formZ = as.formula(~size + small), formE = as.formula(~ small + size_min_scale), hold = 0)
dl <- left_censor(dl, U = exp(-1))

stan_file <- ('~/Dropbox/projects/precip_prediction/code/analysis/growth/censored_gamma.stan')
stan(stan_file, data = dl, chains = 1, iter = 2, cores = 4)
myfit <- stan(stan_file, data = dl, chains = 4, thin = 4, iter = 2000, cores = 4, seed = 2, init = 'random', init_r = 2)

saveRDS(myfit, '~/Dropbox/mygammafit2.RDS')
myfit <- readRDS('~/Dropbox/mygammafit2.RDS')

Y_obs <- dl$Y_obs
Y <- dl$Y
log_Y <- log(Y)
shinystan::launch_shinystan(myfit)

Y_hat <- extract(myfit, 'Y_hat')$Y_hat
plot(log(Y), log(Y_hat[10,]))
plot(Y, Y_hat[10, ])
abline(0,1)

plot(dl$X[,2], log(Y))
points(dl$X[,2], log(Y_hat[34,]), col = 'red')


sum_myfit <- summary(myfit)
sum_myfit <- sum_myfit$c_summary[,,c(1,4)] # choose converged chains 

traceplot(myfit, 'theta')

Y <- dl$Y
log_Y <- log(Y)
shinystan::launch_shinystan(myfit)


plot( extract(myfit,'phi_scale[1]')$`phi_scale[1]` )
plot( extract(myfit,'phi_scale[2]')$`phi_scale[2]` )
plot( extract(myfit,'phi_scale[3]')$`phi_scale[3]` )

plot(extract(myfit,'beta')$beta[,2])


Y_obs <- dl$Y_obs
X <- dl$X[, 2] 
plot(exp(X), Y)
points(exp(X), extract(myfit, 'Y_hat')$Y_hat[8, ], col = 'blue')

plot(X, log(Y))
points(X, log(extract(myfit, 'Y_hat')$Y_hat[40, ]), col = 'blue')
abline(h = log(min(dl$Y)))




#

dat <- data.frame(y  = dl$Y, x  = dl$X[,2], size_min = dl$size_min)
dat <- dat[order(dat$x), ]
plot( dat$x, dat$y)
abline(0,1)

dat$size_class <- ceiling(seq_along(dat$x)/100)
dat <- dat %>% group_by(size_min, size_class) %>% mutate( Rsd = sd(Rs), xmean = mean(x), ymean = mean(y), n = n())

ggplot(data = dat, aes( x = factor(size_class), y = y, color = factor(size_min))) + geom_point() 
ggplot(data = dat, aes( x = factor(size_class), y = x, color = factor(size_min))) + geom_point()
ggplot(data = dat, aes(x = factor(size_class), y= Rsd, color = factor(size_min))) + geom_point() + geom_smooth( se = F, method = 'lm') 

ggplot(data = dat, aes(x = Rs)) + geom_density()  + facet_wrap(~size_class)




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
