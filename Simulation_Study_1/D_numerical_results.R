nobs     <- c(10,25,50)
ngroups  <- c(1,2,3)*2

library(tidyverse)
library(raster)

RESULTS =c()
for(en in 1:3){
  for(gr in 1:3){
  
    name <- paste0("Simulation_Study_1/RUNS/SIM1skb_ngroups_",gr,"_samplesize_",en,".RDS")
    ch=readRDS(name)

    Q=lapply(ch,function(X){
    lapply(X, function(Y) extract_numb_v2(Y)  )
    })

    Q2 = lapply(Q,function(t) (do.call(rbind,t)))
    R1 = array(NA,dim = c(5,5,50))
    for(i in 1:50){
        R1[,,i] = as.matrix(Q2[[i]])
    }
    a1 = apply(R1,c(1,2),mean)
    a2 = apply(R1,c(1,2),sd)

    R2 = matrix(NA,5,10)
    R2[,c(1,3,5,7,9)] = a1
    R2[,c(1,3,5,7,9)+1] = a2

    pp = c(0,0.25,0.50,0.75,"Beta")
    colnames(R2) = c("meanK*","sdK*","meanL*","sdL*","meanKmax","sdKmax","meanLmax","sdLmax","meanSecs","sdSecs")
    RES = cbind(data.frame(p = pp,n = nobs[en],J = ngroups[gr]),R2)
    RESULTS = rbind(RESULTS,RES)
  cat(gr)
    }
}


saveRDS(RESULTS,"Simulation_Study_1/RDS/skb_num_results.RDS")

# -------------------------------------------------------------------------


nobs     <- c(10,25,50)
ngroups  <- c(1,2,3)*2

library(tidyverse)
library(raster)

RESULTS =c()
for(en in 1:3){
  for(gr in 1:3){
    
    name <- paste0("Simulation_Study_1/RUNS/SIM2pbp_ngroups_",gr,"_samplesize_",en,".RDS")
    ch=readRDS(name)
    
    Q=lapply(ch,function(X){
      lapply(X, function(Y) extract_numb_v2(Y)  )
    })
    
    Q2 = lapply(Q,function(t) (do.call(rbind,t)))
    R1 = array(NA,dim = c(3,5,50))
    for(i in 1:50){
      R1[,,i] = as.matrix(Q2[[i]])
    }
    a1 = apply(R1,c(1,2),mean)
    a2 = apply(R1,c(1,2),sd)
    
    R2 = matrix(NA,3,10)
    R2[,c(1,3,5,7,9)] = a1
    R2[,c(1,3,5,7,9)+1] = a2
    
    pp = c(1,0.50,0.10)
    colnames(R2) = c("meanK*","sdK*","meanL*","sdL*","meanKmax","sdKmax","meanLmax","sdLmax","meanSecs","sdSecs")
    RES = cbind(data.frame(p = pp,n = nobs[en],J = ngroups[gr]),R2)
    RESULTS = rbind(RESULTS,RES)
    cat(gr)
  }
}
saveRDS(RESULTS,"Simulation_Study_1/RDS/2pbp_num_results.RDS")








library(tidyverse)

t1 = readRDS("Simulation_Study_1/RDS/skb_num_results.RDS")
t2 = readRDS("Simulation_Study_1/RDS/2pbp_num_results.RDS")

names(t1)

plot(t1$`meanLmax`)
t1 %>% as_tibble %>% mutate(n=as.character(n), J=as.character(J))%>% mutate_if(is.numeric,~format(round(., 3), nsmall = 3)) %>%  mutate(meanLmax = as.numeric(meanLmax)) %>% 
  mutate(Kstar = paste0(`meanK*`," (",`sdK*`,")"),
         Lstar = paste0(`meanL*`," (",`sdL*`,")"),
         Kmax = paste0(`meanKmax`," (",`sdKmax`,")"),
         Lmax = paste0(`meanLmax`," (",`sdLmax`,")"),
         Secs = paste0(`meanSecs`," (",`sdSecs`,")"),
         ) %>% dplyr::select(p,n,J,Kstar,Lstar,Kmax,Lmax,Secs) %>% 
knitr::kable(format = "latex",booktabs=TRUE,digits = 3)




t2 %>% as_tibble %>% mutate(n=as.character(n), J=as.character(J))%>% mutate_if(is.numeric,~format(round(., 3), nsmall = 3)) %>%  mutate(meanLmax = as.numeric(meanLmax)) %>% 
  mutate(Kstar = paste0(`meanK*`," (",`sdK*`,")"),
         Lstar = paste0(`meanL*`," (",`sdL*`,")"),
         Kmax = paste0(`meanKmax`," (",`sdKmax`,")"),
         Lmax = paste0(`meanLmax`," (",`sdLmax`,")"),
         Secs = paste0(`meanSecs`," (",`sdSecs`,")"),
  ) %>% dplyr::select(p,n,J,Kstar,Lstar,Kmax,Lmax,Secs) %>% 
  knitr::kable(format = "latex",booktabs=TRUE,digits = 3)

