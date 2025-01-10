library(gecam2)
library(LaplacesDemon)
library(philentropy)

nobs     <- c(10,25,50)
ngroups  <- c(1,2,3)
nsim  <- 50
niter <- 10000
f1 <- function(x) dnorm(x,-5)
f2 <- function(x) dnorm(x, 5)

# Different skips --------------------------------------------------------

# RUN SKIP-BREAK
single_run_five_skb = function(i){
  res <- list()
  for (seed in 1:5) {
    set.seed(seed)
    res[[seed]] <- gibbs_gecam(Y = Ys[[i]], G = Gs[[i]],
                         s1 = 1, s2 = 1,
                         p = ifelse(seed == 5, 0.5, c(0, 0.25, 0.5, 0.75)[seed]),
                         nsim = niter,
                         sd1 = 1, sd2 = 1,
                         L = 20, K = 10,
                         p_random = ifelse(seed == 5, TRUE, FALSE))
  }
  return(res)
}


# RUN 2PBP
single_run_three_2pbp = function(i){
  res <- list()
  for (seed in 1:3) {
    set.seed(seed+10)
    res[[seed]] <- gibbs_gecam(Y = Ys[[i]], G = Gs[[i]],
                         s1 = SS[[seed]], s2 = SS[[seed]],
                         p = 0,
                         nsim = niter,
                         sd1 = 1, sd2 = 1,
                         L = 20, K = 10,
                         p_random = FALSE)
  }
  return(res)
}


# DENS EST
extract_dens <- function(RES, yseq = seq(-10,10,by=.01), subject = 1){

  nsim <- length(RES)
  DEN  <- array(NA,c(length(yseq),length(RES)))
  
  DEN <-  do.call(cbind,lapply(RES, function(x) avg_dens(x, howmany = 1000,
                                                                s = subject,
                                                                yseq = yseq)[,1]))
  
  list(x = yseq, dens = DEN)
}



extract_numb_v2 <- function(Y){

  
  uesse = apply(Y$ES,1,function(y) length(unique(y)))
  uemme = apply(Y$EM,1,function(y) length(unique(y)))
  messe = apply(Y$ES,1,function(y) max(unique(y)))
  memme = apply(Y$EM,1,function(y) max(unique(y)))
  
  modeS = raster::modal(uesse)
  modeM = raster::modal(uemme)
  maxS = mean(messe)
  maxM = mean(memme)
  secs = as.numeric(Y$elapsed, units = "secs")
  c(es = modeS, em = modeM, maxes = maxS, maxem = maxM, secs = secs)
}



