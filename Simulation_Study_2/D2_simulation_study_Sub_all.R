library(gecam2)
library(LaplacesDemon)
library(philentropy)
library(tidyverse)
source("Simulation_Study_2/A2_simulation_study_functions.R")
theme_set(theme_bw())
# Results -----------------------------------------------------------------

## extract the densities -------------------------------------------------------

data_skip= c()
data_2pbp= c()
for(en in 1:2){
  for(gr in 1:2){
 
 d1 = readRDS(paste0("Simulation_Study_2/DENS/dens_list_skip_ngroups_",gr,"_samplesize_",en,"_sub_ALL.RDS"))
 d2 = readRDS(paste0("Simulation_Study_2/DENS/dens_list_2pbp_ngroups_",gr,"_samplesize_",en,"_sub_ALL.RDS"))
 
 n_groups = length(d1) 
 for(j in 1:n_groups){
   
   for(i in 1:nsim){
     dada1 = d1[[j]][[i]]$dens %>% as.data.frame %>%  mutate(en = en, 
                                                             gr = gr, 
                                                             sim =i, 
                                                             group = j, 
                                                             pop = ifelse(j<=n_groups/2,1,2),
                                                             y = d1[[j]][[i]]$x,
                                                             truth = case_when( j<=n_groups/2 ~ f1(d1[[j]][[i]]$x),
                                                                                j >n_groups/2 ~ f2(d1[[j]][[i]]$x)))                 
     
     dada2 = d2[[j]][[i]]$dens %>% as.data.frame %>%  mutate(en = en, 
                                                             gr = gr, 
                                                             sim =i, 
                                                             group = j, 
                                                             pop = ifelse(j<=n_groups/2,1,2),
                                                             y =d2[[j]][[i]]$x,
                                                             truth = case_when( j<=n_groups/2 ~ f1(d1[[j]][[i]]$x),
                                                                                j >n_groups/2 ~ f2(d1[[j]][[i]]$x)))
                                                                              
     data_skip = rbind(data_skip,dada1)
     data_2pbp = rbind(data_2pbp,dada2)
     cat(".")
     }
    } 
 cat("\n------\n")
  } 
}
 
saveRDS(data_skip,"Simulation_Study_2/RDS/data_skip.RDS")
saveRDS(data_2pbp,"Simulation_Study_2/RDS/data_2pbp.RDS")

# -------------------------------------------------------------------------

data_skip = readRDS("Simulation_Study_2/RDS/data_skip.RDS")
data_2pbp = readRDS("Simulation_Study_2/RDS/data_2pbp.RDS")

data_skip2 = data_skip %>% reshape2::melt(c("en","gr","sim","group","truth","y","pop")) 
data_2pbp2 = data_2pbp %>% reshape2::melt(c("en","gr","sim","group","truth","y","pop")) 

step = .01

data_skip2 = data_skip2 %>% 
               dplyr::select(-y) %>% 
               group_by(en,gr,sim,variable,group,pop) %>% 
               reframe(KL = KLD(value,truth)$sum.KLD.px.py,
                       JS = philentropy::JSD(rbind(value,truth)),
                       TV = .5 * sum(abs(value-truth)*step)
                       )
data_2pbp2 = data_2pbp2 %>% 
  dplyr::select(-y) %>% 
  group_by(en,gr,sim,variable,group,pop) %>% 
  reframe(KL = KLD(value,truth)$sum.KLD.px.py,
          JS = philentropy::JSD(rbind(value,truth)),
          TV = .5 * sum(abs(value-truth)*step)
  ) 



data_skip3= data_skip2 %>% dplyr::select(-group) %>% 
  group_by(en,gr,sim,variable) %>% reframe(mKL=mean(KL),
                                           mJS=mean(JS),
                                           mTV=mean(TV)
  )

data_2pbp3= data_2pbp2 %>% dplyr::select(-group) %>% 
  group_by(en,gr,sim,variable) %>% reframe(mKL=mean(KL),
                                           mJS=mean(JS),
                                           mTV=mean(TV)
  )



# SKIP --------------------------------------------------------------------

nobs     <- c(10,25,50)
ngroups  <- c(1,2,3)

D1 = data_skip3 %>% mutate(
             en = case_when(en==1 ~ paste("n =",nobs[1]),
                            en==2 ~ paste("n =",nobs[2]),
                            en==3 ~ paste("n =",nobs[3])),
             gr = case_when(gr==1 ~ paste("J =",2*ngroups[1]),
                            gr==2 ~ paste("J =",2*ngroups[2]),
                            gr==3 ~ paste("J =",2*ngroups[3])
           ))

cols <- c("tomato3","steelblue3")

D1 <-D1 %>%
             mutate(Var1a = factor(case_when(variable == "V1" ~ "CAM",
                                      variable == "V2" ~ "0.25",
                                      variable == "V3" ~ "0.50",
                                      variable == "V4" ~ "0.75",
                                      variable == "V5" ~ "Beta",
),levels = c("CAM","0.25","0.50","0.75","Beta")))

as_tibble(D1)


ggplot(D1 %>% as_tibble())+
#  geom_vline(xintercept = 1,col=1,lty=3)+
  geom_boxplot(aes(x=(Var1a),
                   y=mKL))+
facet_grid(en~gr,scales = "free_y")+theme_bw()+
  scale_fill_manual("",values = cols)+
  ylab("KL Divergence")+theme(legend.position = "bottom",
                           text = element_text(size=16),
                           axis.text.x = element_text(angle = 0))+
  xlab("p")
  #ggtitle("Skip-Breaking(p) - First subpopulation")
ggview::ggview(h=9,w=12)

ggsave("Simulation_Study_2/OUTPUT/D2_skip_KL.pdf",h=9,w=12)
ggsave("Simulation_Study_2/OUTPUT/D2_skip_KL.eps",h=9,w=12)


D1 %>% filter(en == "n = 10") %>%  group_by(Var1a,en,gr) %>% reframe(mean(mTV),median(mTV),sd(mTV))


ggplot(D1 %>% as_tibble())+
  #  geom_vline(xintercept = 1,col=1,lty=3)+
  geom_boxplot(aes(x=(Var1a),
                   y=mJS))+
  facet_grid(en~gr,scales = "free_y")+theme_bw()+
  scale_fill_manual("",values = cols)+
  ylab("JS Divergence")+theme(legend.position = "bottom",
                              text = element_text(size=16),
                              axis.text.x = element_text(angle = 0))+
  xlab("p")
#ggtitle("Skip-Breaking(p) - First subpopulation")
ggview::ggview(h=9,w=12)

ggsave("Simulation_Study_2/OUTPUT/D2_skip_JS.pdf",h=9,w=12)
ggsave("Simulation_Study_2/OUTPUT/D2_skip_JS.eps",h=9,w=12)


ggplot(D1 %>% as_tibble())+
  #  geom_vline(xintercept = 1,col=1,lty=3)+
  geom_boxplot(aes(x=(Var1a),
                   y=mTV))+
  facet_grid(en~gr,scales = "free_y")+theme_bw()+
  scale_fill_manual("",values = cols)+
  ylab("TV Distance")+theme(legend.position = "bottom",
                              text = element_text(size=16),
                              axis.text.x = element_text(angle = 0))+
  xlab("p")
#ggtitle("Skip-Breaking(p) - First subpopulation")
ggview::ggview(h=9,w=12)

ggsave("Simulation_Study_2/OUTPUT/D2_skip_TV.pdf",h=9,w=12)
ggsave("Simulation_Study_2/OUTPUT/D2_skip_TV.eps",h=9,w=12)
# 2PBP --------------------------------------------------------------------

nobs     <- c(10,25,50)
ngroups  <- c(1,2,3)

D1 = data_2pbp3 %>% mutate(
  en = case_when(en==1 ~ paste("n =",nobs[1]),
                 en==2 ~ paste("n =",nobs[2]),
                 en==3 ~ paste("n =",nobs[3])),
  gr = case_when(gr==1 ~ paste("J =",2*ngroups[1]),
                 gr==2 ~ paste("J =",2*ngroups[2]),
                 gr==3 ~ paste("J =",2*ngroups[3])
  ))

cols <- c("tomato3","steelblue3")

D1 <-D1 %>%
  mutate(Var1a = factor(case_when(variable == "V1" ~ "CAM",
                                  variable == "V2" ~ "0.50",
                                  variable == "V3" ~ "0.10",
  ),levels = c("CAM","0.50","0.10")))

as_tibble(D1)


ggplot(D1 %>% as_tibble())+
  #  geom_vline(xintercept = 1,col=1,lty=3)+
  geom_boxplot(aes(x=(Var1a),
                   y=mKL))+
  facet_grid(en~gr,scales = "free_y")+theme_bw()+
  scale_fill_manual("",values = cols)+
  ylab("KL Divergence")+theme(legend.position = "bottom",
                              text = element_text(size=16),
                              axis.text.x = element_text(angle = 0))+
  xlab("s")
#ggtitle("Skip-Breaking(p) - First subpopulation")
ggview::ggview(h=9,w=12)

ggsave("Simulation_Study_2/OUTPUT/D2_2pbp_KL.pdf",h=9,w=12)
ggsave("Simulation_Study_2/OUTPUT/D2_2pbp_KL.eps",h=9,w=12)


ggplot(D1 %>% as_tibble())+
  #  geom_vline(xintercept = 1,col=1,lty=3)+
  geom_boxplot(aes(x=(Var1a),
                   y=mJS))+
  facet_grid(en~gr,scales = "free_y")+theme_bw()+
  scale_fill_manual("",values = cols)+
  ylab("JS Divergence")+theme(legend.position = "bottom",
                              text = element_text(size=16),
                              axis.text.x = element_text(angle = 0))+
  xlab("s")
#ggtitle("Skip-Breaking(p) - First subpopulation")
ggview::ggview(h=9,w=12)

ggsave("Simulation_Study_2/OUTPUT/D2_2pbp_JS.pdf",h=9,w=12)
ggsave("Simulation_Study_2/OUTPUT/D2_2pbp_JS.eps",h=9,w=12)


ggplot(D1 %>% as_tibble())+
  #  geom_vline(xintercept = 1,col=1,lty=3)+
  geom_boxplot(aes(x=(Var1a),
                   y=mTV))+
  facet_grid(en~gr,scales = "free_y")+theme_bw()+
  scale_fill_manual("",values = cols)+
  ylab("TV Distance")+theme(legend.position = "bottom",
                            text = element_text(size=16),
                            axis.text.x = element_text(angle = 0))+
  xlab("s")
#ggtitle("Skip-Breaking(p) - First subpopulation")
ggview::ggview(h=9,w=12)

ggsave("Simulation_Study_2/OUTPUT/D2_2pbp_TV.pdf",h=9,w=12)
ggsave("Simulation_Study_2/OUTPUT/D2_2pbp_TV.eps",h=9,w=12)
