library(tidyverse)
library(patchwork)
nobs     <- c(10,25,50)
ngroups  <- c(1,2,3)

data_skip = readRDS("Simulation_Study_1/RDS/data_skip.RDS")
data_2pbp = readRDS("Simulation_Study_1/RDS/data_2pbp.RDS")



melt_skip = data_skip %>%
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop")) %>% filter(variable %in% c("V1","V3")) %>%
  filter(gr==1,en==1)
melt_2pbp = data_skip %>%
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop")) %>% filter(variable %in% c("V2")) %>%
  filter(gr==1,en==1)

melt_ = rbind(melt_skip,melt_2pbp) %>% mutate(variable =
                                                case_when(variable == "V1" ~ "CAM",
                                                        variable == "V3" ~ "SKBP(p = 0.50)",
                                                        variable == "V2" ~ "2PBP(s = 0.50)"
)) %>% mutate(variable = factor(variable,levels= c("CAM","SKBP(p = 0.50)","2PBP(s = 0.50)"))) %>%
  mutate(pop = paste("Subpopulation",pop))


meltmean = melt_ %>% dplyr::select(-gr,-en) %>%
  group_by(truth, y, variable, pop, group) %>%
  reframe(md = mean(value))


SS=melt_skip %>% filter(variable=="V1", group==1)

plot(SS$y,SS$value,pch=".")

ggplot()+theme_bw()+
  geom_vline(xintercept = c(-5,5),lty=3,col="tomato3") +
  geom_path(data = melt_, aes(x=y, y=value, group = sim),
            col="lightgray",alpha=.9,lwd=.5)+
  geom_line(data = melt_, aes(x=y,y=truth),
            col="black",lwd=.5,lty=1)+
  geom_line(data= meltmean,
            aes(x=y,y=md),col="steelblue3",lwd=.5)+
  facet_grid((pop~variable))  +
  theme(text = element_text(size=18))+
  ylab("Density")+ggview::canvas(h=6,w=9)
ggsave("Simulation_Study_1/OUTPUT/FIG4.pdf",h=6,w=9)
ggsave("Simulation_Study_1/OUTPUT/FIG4.eps",h=6,w=9,device = cairo_ps)







# all ---------------------------------------------------------------------

melt_skip = data_skip %>%
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop")) %>%
  mutate(variable = factor(case_when(variable == "V1" ~ "p = 0 (CAM)",
                                  variable == "V2" ~ "p = 0.25",
                                  variable == "V3" ~ "p = 0.50",
                                  variable == "V4" ~ "p = 0.75",
                                  variable == "V5" ~ "p ~ Beta",
  ),levels = c("p = 0 (CAM)","p = 0.25","p = 0.50","p = 0.75","p ~ Beta")))


meltmean = melt_skip  %>%
  group_by(truth, y, variable, pop, gr, en) %>%
  reframe(md = mean(value))


plot(meltmean$y,meltmean$md,pch=".")

m1 = meltmean %>% filter(pop==1) %>%  mutate(
  en = case_when(en==1 ~ paste("n =",nobs[1]),
                 en==2 ~ paste("n =",nobs[2]),
                 en==3 ~ paste("n =",nobs[3])),
  gr = case_when(gr==1 ~ paste("J =",2*ngroups[1]),
                 gr==2 ~ paste("J =",2*ngroups[2]),
                 gr==3 ~ paste("J =",2*ngroups[3])
  ))


ggplot()+theme_bw()+
  geom_vline(xintercept = c(-5,5),lty=3,col="tomato3") +
  geom_line(data = m1, aes(x=y,y=truth,group=variable),
            col="black",lwd=.5,lty=1)+
  geom_line(data= m1,
            aes(x=y,y=md,group=variable,col=variable),lwd=.5)+
  facet_grid((en~gr))  +
  theme(text = element_text(size=24),legend.position = "bottom")+
  ylab("Density")+scale_color_brewer("SKBP(p)  ",palette = "Set1")

ggsave("Simulation_Study_1/OUTPUT/FIGS8a_skb_pop1.pdf",h=10,w=15)
ggsave("Simulation_Study_1/OUTPUT/FIGS8a_skb_pop1.eps",h=10,w=15,device = cairo_ps)



# all ---------------------------------------------------------------------

melt_2pbp = data_2pbp %>%
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop")) %>%
  mutate(variable = factor(case_when(variable == "V1" ~ "s = 1 (CAM)",
                                     variable == "V2" ~ "s = 0.50",
                                     variable == "V3" ~ "s = 0.10"
  ),levels = c("s = 1 (CAM)","s = 0.50","s = 0.10")))


meltmean = melt_2pbp  %>%
  group_by(truth, y, variable, pop, gr, en) %>%
  reframe(md = mean(value))


plot(meltmean$y,meltmean$md,pch=".")

m1 = meltmean %>% filter(pop==1) %>%  mutate(
  en = case_when(en==1 ~ paste("n =",nobs[1]),
                 en==2 ~ paste("n =",nobs[2]),
                 en==3 ~ paste("n =",nobs[3])),
  gr = case_when(gr==1 ~ paste("J =",2*ngroups[1]),
                 gr==2 ~ paste("J =",2*ngroups[2]),
                 gr==3 ~ paste("J =",2*ngroups[3])
  ))


ggplot()+theme_bw()+
  geom_vline(xintercept = c(-5,5),lty=2) +
  geom_line(data = m1, aes(x=y,y=truth,group=variable),
            col="black",lwd=.5)+
  geom_line(data= m1,
            aes(x=y,y=md,group=variable,col=variable),lwd=.5)+
  facet_grid((en~gr))  +
  theme(text = element_text(size=18),legend.position = "bottom")+
  ylab("Density")+scale_color_brewer("2PBP(s)  ",palette = "Set1")

ggsave("Simulation_Study_1/OUTPUT/FIGS9a_2pb_pop1.pdf",h=10,w=15)
ggsave("Simulation_Study_1/OUTPUT/FIGS9a_2pb_pop1.eps",h=10,w=15,device = cairo_ps)





# all ---------------------------------------------------------------------

melt_skip = data_skip %>%
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop")) %>%
  mutate(variable = factor(case_when(variable == "V1" ~ "p = 0 (CAM)",
                                     variable == "V2" ~ "p = 0.25",
                                     variable == "V3" ~ "p = 0.50",
                                     variable == "V4" ~ "p = 0.75",
                                     variable == "V5" ~ "p ~ Beta",
  ),levels = c("p = 0 (CAM)","p = 0.25","p = 0.50","p = 0.75","p ~ Beta")))


meltmean = melt_skip  %>%
  group_by(truth, y, variable, pop, gr, en) %>%
  reframe(md = mean(value))


plot(meltmean$y,meltmean$md,pch=".")

m1 = meltmean %>% filter(pop==2) %>%  mutate(
  en = case_when(en==1 ~ paste("n =",nobs[1]),
                 en==2 ~ paste("n =",nobs[2]),
                 en==3 ~ paste("n =",nobs[3])),
  gr = case_when(gr==1 ~ paste("J =",2*ngroups[1]),
                 gr==2 ~ paste("J =",2*ngroups[2]),
                 gr==3 ~ paste("J =",2*ngroups[3])
  ))


ggplot()+theme_bw()+
  geom_vline(xintercept = c(-5,5),lty=3,col="tomato3") +
  geom_line(data = m1, aes(x=y,y=truth,group=variable),
            col="black",lwd=.5,lty=1)+
  geom_line(data= m1,
            aes(x=y,y=md,group=variable,col=variable),lwd=.5)+
  facet_grid((en~gr))  +
  theme(text = element_text(size=24),legend.position = "bottom")+
  ylab("Density")+scale_color_brewer("SKBP(p)  ",palette = "Set1")

ggview::ggview(h=10,w=15)
ggsave("Simulation_Study_1/OUTPUT/FIGS8b_supp_skb_pop2.pdf",h=10,w=15)
ggsave("Simulation_Study_1/OUTPUT/FIGS8b_supp_skb_pop2.eps",h=10,w=15,device = cairo_ps)



# all ---------------------------------------------------------------------

melt_2pbp = data_2pbp %>%
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop")) %>%
  mutate(variable = factor(case_when(variable == "V1" ~ "s = 1 (CAM)",
                                     variable == "V2" ~ "s = 0.50",
                                     variable == "V3" ~ "s = 0.10"
  ),levels = c("s = 1 (CAM)","s = 0.50","s = 0.10")))


meltmean = melt_2pbp  %>%
  group_by(truth, y, variable, pop, gr, en) %>%
  reframe(md = mean(value))


plot(meltmean$y,meltmean$md,pch=".")

m1 = meltmean %>% filter(pop==2) %>%  mutate(
  en = case_when(en==1 ~ paste("n =",nobs[1]),
                 en==2 ~ paste("n =",nobs[2]),
                 en==3 ~ paste("n =",nobs[3])),
  gr = case_when(gr==1 ~ paste("J =",2*ngroups[1]),
                 gr==2 ~ paste("J =",2*ngroups[2]),
                 gr==3 ~ paste("J =",2*ngroups[3])
  ))


ggplot()+theme_bw()+
  geom_vline(xintercept = c(-5,5),lty=2) +
  geom_line(data = m1, aes(x=y,y=truth,group=variable),
            col="black",lwd=.5)+
  geom_line(data= m1,
            aes(x=y,y=md,group=variable,col=variable),lwd=.5)+
  facet_grid((en~gr))  +
  theme(text = element_text(size=18),legend.position = "bottom")+
  ylab("Density")+scale_color_brewer("2PB(s)  ",palette = "Set1")

ggview::ggview(h=10,w=15)
ggsave("Simulation_Study_1/OUTPUT/FIGS9b_supp_2pb_pop2.pdf",h=10,w=15)
ggsave("Simulation_Study_1/OUTPUT/FIGS9b_supp_2pb_pop2.eps",h=10,w=15,device = cairo_ps)
