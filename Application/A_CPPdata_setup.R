draw_dens <- function(res,
                      howmany = 10,
                      yseq = seq(-15, 15, by = .1),
                      s = 1) {
  RES = matrix(NA, howmany, length(yseq))
  nsim = ncol(res$PI)
  L = nrow(res$TH)
  for (t in (nsim - howmany + 1):nsim) {
    RES[t - (nsim - howmany), ] <- rowSums(sapply(1:L,
                                                  function(x)
                                                    res$OM[x, res$ES[t, s], t] * dnorm(yseq, res$TH[, , t][x, 1],
                                                                                       sqrt(res$TH[, , t][x, 2]))))
  }
  
  plot(RES[t - (nsim - howmany), ] ~ yseq, type = "l")
  for (t in (nsim - howmany + 1):nsim) {
    lines(RES[t - (nsim - howmany), ] ~ yseq, col = "gray")
  }
  lines(
    colMeans(RES) ~ yseq,
    col = "darkblue",
    type = "b",
    cex = .4
  )
}

# Libraries -------------------------------------------------------------------------

library(gecam2)
library(BNPmix)
library(tidyverse)


# Read Data ---------------------------------------------------------------

data(CPP)
plot(CPP$gest,col=CPP$hosp, pch="x")
table(CPP$hosp)

set.seed(124)

# Preprocessing -----------------------------------------------------------

CPPsub = tibble(CPP) %>% #filter(smoke==1) %>%
  arrange(hosp)
hist(CPPsub$weight)
#View(CPP)
Y <- c(CPPsub$weight)
G <- as.numeric(c(CPPsub$hosp))
length(Y)
table(G)

pairs(CPP,pch=".",col=CPP$smoke)
plot(Y,col = G, pch="x")