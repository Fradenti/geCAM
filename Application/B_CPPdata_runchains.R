

set.seed(250892) 

Rskb <- gibbs_gecam(Y = (Y),
                  G = G,
                  s1 = 1,
                  s2 = 1,
                  p = 0.5,
                  p_random = FALSE,
                  nsim = 20000,
                  sd1 = 1,
                  sd2 = 1,
                  L = 50,
                  K = 30,
                  m0 = mean(Y),
                  tau = .01,
                  lambda0 = 1,
                  gamma0 = 4)

saveRDS(Rskb,"Application/RDS/run_CPP_SKPB.RDS")

set.seed(090994) 

Rcam <- gibbs_gecam(Y = (Y),
                    G = G,
                    s1 = 1,
                    s2 = 1,
                    p = 0,
                    p_random = FALSE,
                    nsim = 20000,
                    sd1 = 1,
                    sd2 = 1,
                    L = 50,
                    K = 30,
                    m0 = mean(Y),
                    tau = .01,
                    lambda0 = 1,
                    gamma0 = 4)
saveRDS(Rcam,"Application/RDS/run_CPP_CAM.RDS")
