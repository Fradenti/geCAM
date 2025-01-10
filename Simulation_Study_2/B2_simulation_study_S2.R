
library(gecam2)
library(LaplacesDemon)
library(philentropy)
source("Simulation_Study_2/A2_simulation_study_functions.R")
# Run models --------------------------------------------------------------

# Define parameters
for(en in 1:2){
  for(gr in 1:2){

    J <- ngroups[gr]
    n <- nobs[en]
    set.seed(1234 + en + gr)
    # Generate multiple datasets
    Ys <- list()
    Gs <- list()

    for(k in 1:nsim){
      Ys[[k]] <- c(replicate(J,c(
        rnorm(n/ 3, -5),
        rnorm(n/ 3, 0),
        rnorm(n/ 3, 5))),
        replicate(J,c(
          rnorm(n/2,  0),
          rnorm(n/2,  3))))
      Gs[[k]] <- rep(1:(2*J), each = c(n))
    }
    plot(Ys[[nsim]],col=Gs[[nsim]])

    RES <- parallel::mclapply(1:nsim, function(t) single_run_five_skb(t),
                              mc.cores = 10)
    name <- paste0("Simulation_Study_2/RUNS/SIM1skb_ngroups_",gr,"_samplesize_",en,".RDS")
    saveRDS(RES,name)
    rm(RES)
    cat(en)
  }
}


# Different s1, s2 --------------------------------------------------------

SS = c(1,.5,.1)

# Define parameters
for(en in 1:2){
  for(gr in 1:2){
    J <- ngroups[gr]
    n <- nobs[en]
    set.seed(1234 + en + gr)
    # Generate multiple datasets
    Ys <- list()
    Gs   <- list()

    for(k in 1:nsim){
      Ys[[k]] <- c(replicate(J,c(
        rnorm(n/ 3, -5),
        rnorm(n/ 3, 0),
        rnorm(n/ 3, 5))),
        replicate(J,c(
          rnorm(n/2,  0),
          rnorm(n/2,  3))))
      Gs[[k]] <- rep(1:(2*J), each = c(n))
    }
    RES <- parallel::mclapply(1:nsim,
                              function(t) single_run_three_2pbp(t),
                              mc.cores = 10)
    name <- paste0("Simulation_Study_2/RUNS/SIM2pbp_ngroups_",gr,"_samplesize_",en,".RDS")
    saveRDS(RES,name)
    rm(RES)
  }
}



