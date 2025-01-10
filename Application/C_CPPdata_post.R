library(salso)
source("Application/A_CPPdata_setup.R")
# Postprocessing -------------------------------------------------------------------------

Rskb <- readRDS("Application/RDS/run_CPP_SKPB.RDS")
Rcam <- readRDS("Application/RDS/run_CPP_CAM.RDS")

uniq_ES_CAM <- ts(apply(Rcam$ES,1,function(x) length(unique(x))))
uniq_ES_SKB <- ts(apply(Rskb$ES,1,function(x) length(unique(x))))

max_ES_CAM <- ts(apply(Rcam$ES,1,function(x) max(unique(x))))
max_ES_SKB <- ts(apply(Rskb$ES,1,function(x) max(unique(x))))


par(mfrow=c(1,1))
plot(ts(max_ES_CAM))
lines(ts(max_ES_SKB),col=2)



plot(ts(uniq_ES_CAM))
lines(ts(uniq_ES_SKB),col=2)
plot( table(uniq_ES_CAM))
lines(table(uniq_ES_SKB),col=2)

uniq_EM_CAM <- ts(apply(Rcam$EM,1,function(x) length(unique(x))))
uniq_EM_SKB <- ts(apply(Rskb$EM,1,function(x) length(unique(x))))
max_EM_CAM <- ts(apply(Rcam$EM,1,function(x) max(unique(x))))
max_EM_SKB <- ts(apply(Rskb$EM,1,function(x) max(unique(x))))


plot( ts(uniq_EM_CAM))
lines(ts(uniq_EM_SKB),col=2)
plot( density(uniq_EM_CAM))
lines(density(uniq_EM_SKB),col=2)

plot( ts(max_EM_SKB),col=2)
lines( ts(max_EM_CAM))

plot( table(max_EM_SKB),col=2)
lines( table(max_EM_CAM))


plot(table(uniq_EM_CAM))
lines(table(uniq_EM_SKB),col=2)

plot(table(uniq_ES_CAM))
plot(table(uniq_ES_SKB))

hist(CPPsub$weight,breaks = 50)
par(mfrow=c(1,2))
draw_dens(Rskb,yseq = seq(-25,150,by=.1),s = 10,howmany = 400)
draw_dens(Rcam,yseq = seq(-25,150,by=.1),s = 10,howmany = 400)
draw_dens(Rskb,yseq = seq(-25,150,by=.1),s = 2,howmany = 400)
draw_dens(Rcam,yseq = seq(-25,150,by=.1),s = 2,howmany = 400)

boxplot(t(Rskb$OM[,,1]))
boxplot(t(Rcam$OM[,,1]))
par(mfrow=c(1,1))

# Partitions --------------------------------------------------------------

ps1 = salso::psm((Rskb$ES))
superheat::superheat(ps1,pretty.order.cols = TRUE,pretty.order.rows = TRUE)
heatmap(ps1)
pscam = salso::psm((Rcam$ES))
superheat::superheat(pscam,pretty.order.cols = TRUE,pretty.order.rows = TRUE)
heatmap(pscam)

set.seed(1)
clskb = salso::salso((Rskb$ES),nRuns = 2500,
                     maxNClusters = 12,
                     loss = VI(a=1), maxZealousAttempts = 100)
table(clskb)

set.seed(1)
clcam = salso::salso((Rcam$ES),nRuns = 5000,
                     maxNClusters = 12,
                     loss = VI(a=1),
                     maxZealousAttempts = 100)
table(clcam)

dim(Rskb$ES)
dim(Rskb$ES)

par(mfrow=c(1,2))

a1 = avg_dens(Rskb, 500,s = 1,yseq = seq(-25,150,by=0.5))
a2 = avg_dens(Rskb, 500,s = 3,yseq = seq(-25,150,by=0.5))
plot(a1[,1]~a1[,2],col=4,type="l")
for(i in 1:12){
  a = avg_dens(Rskb,howmany = 100,s = i,yseq = seq(-25,150,by=0.5))
  lines(a[,1]~a[,2],col=i)
}

a1 = avg_dens(Rcam, 500,s = 1,yseq = seq(-25,150,by=0.5))
a2 = avg_dens(Rcam, 500,s = 3,yseq = seq(-25,150,by=0.5))
plot(a1[,1]~a1[,2],col=4,type="l")
for(i in 1:12){
  a = avg_dens(Rcam,howmany = 100,s = i,yseq = seq(-25,150,by=0.5))
  lines(a[,1]~a[,2],col="gray")
}

CPP2 <- CPPsub %>% mutate(dcl = rep(clskb,table(G)))
table(CPP2$hosp,CPP2$dcl)

A = c()
for(i in 1:12){
  a = avg_dens(Rskb, howmany = 5000, s = i, yseq = seq(-25,150,by=0.5))
  lines( a[,1] ~ a[,2], col=i)
  A = rbind(A,cbind(a,i,clskb[i]))
}

# CPP2 %>% select(-smoke,-hosp) %>% as_tibble() %>% cor()
ps = heatmap(ps1)
library(patchwork)
p1 <-ggplot(ps1[ps$rowInd,ps$colInd] %>% reshape2::melt())+theme_bw()+
  geom_tile(aes(x=factor(Var1),y=factor(Var2),fill=value))+
  scale_fill_gradient("geCAM (SKBP)     ",low = "white",high = "steelblue3")+
  xlab("Sorted Hospitals")+ylab("Sorted Hospitals")+
  geom_segment(y=0.5,yend=12.6,x=6.5,xend=6.5)+
  geom_segment(x=0.5,xend=12.6,y=6.5,yend=6.5)+
  theme(legend.position = "bottom",text=element_text(size=18),legend.key.width = unit(1,"cm"))
p1+ggview::canvas(h=8,w=8)
ggsave("Application/output/FIG7a_PSM_Rskb.pdf",h=8,w=8)
ggsave("Application/output/FIG7a_PSM_Rskb.eps",h=8,w=8)

cols <- c("tomato3","steelblue3","forestgreen")

p2 <- ggplot(data = A %>% as_tibble() %>% mutate(V4=factor(V4)) )+
  geom_line(aes(x=yseq,
                y=V1,
                col=V4,group=i), lwd = .8)+
  theme_bw()+
  scale_color_manual("DC",values = cols)+
  ylab("Density") + xlab("Birth weight")+
  theme(legend.position = "bottom",text=element_text(size=18),
        legend.key.width = unit(1,"cm"))
p2+facet_wrap(~"geCAM (SKBP)")+ ggview::canvas(h=8,w=8)
ggsave("Application/output/FIG9a_DENS_SKB.pdf",h=8,w=8)
ggsave("Application/output/FIG9a_DENS_SKB.eps",h=8,w=8)

sumCPP2 <- CPP2 %>% group_by(dcl) %>% summarise(med_logdde = median((dde)),
                                                med_loggest = median((gest)))
p3 <- ggplot(data = CPP2 %>% as_tibble())+
  geom_density(aes(x = (gest),
                   #group=hosp,
                   col=factor(dcl)), lwd = .8)+
  geom_vline(data=sumCPP2,aes(xintercept = med_loggest, col=factor(dcl)),lwd=1, lty=4)+
  theme_bw()+
  scale_color_manual("DC",values = cols)+
  ylab("Density") + xlab("Gestational age")+
  theme(text=element_text(size=18),
        legend.position = "bottom",
        legend.key.width = unit(1,"cm"))
p3



p4 <- ggplot(data = CPP2 %>% as_tibble())+
  geom_density(aes(
    x = (dde),
    #group=hosp,
    col=factor(dcl)), lwd = .8)+
  geom_vline(data=sumCPP2,aes(xintercept = med_logdde, col=factor(dcl)),lwd=1,lty=4)+
  theme_bw()+
  scale_color_manual("DC",values = cols)+
  xlab("DDE concentration") + ylab("Density")+
  theme(text=element_text(size=18),
        legend.position = "bottom",
        legend.key.width = unit(1,"cm"))

p4

p3+p4+ggview::canvas(h=5,w=10)+
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave("Application/output/FIG10_kernelsdensX.pdf",h=5,w=10)
ggsave("Application/output/FIG10_kernelsdensX.eps",h=5,w=10)


# CAM ---------------------------------------------------------------------

A = c()
plot(a1[,1]~a1[,2],col=4,type="l")
for(i in 1:12){
  a = avg_dens(Rcam, howmany = 5000, s = i, yseq = seq(-25,150,by=0.5))
  lines( a[,1] ~ a[,2], col=i)
  A = rbind(A,cbind(a,i,clcam[i]))
}

CPP2 <- CPPsub %>% mutate(dcl = rep(clcam,table(G)))
table(CPP2$hosp,CPP2$dcl)

ps = heatmap(pscam)
library(patchwork)
p1 <- ggplot(pscam[ps$rowInd,ps$colInd] %>% reshape2::melt())+theme_bw()+
  geom_tile(aes(x=factor(Var1),y=factor(Var2),fill=value))+
  scale_fill_gradient("CAM      ",low = "white",high = "steelblue3")+
  xlab("Sorted Hospitals")+ylab("Sorted Hospitals")+
  geom_segment(y=0.5,yend=8.5,x=6.5,xend=6.5)+
  geom_segment(x=8.5,xend=0.5,y=6.5,yend=6.5)+
  geom_segment(y=8.5,yend=8.5,x=6.5,xend=12.6)+
  geom_segment(x=8.5,xend=8.5,y=6.5,yend=12.6)+
  theme(legend.position = "bottom",text=element_text(size=18),
        legend.key.width = unit(1,"cm"))
p1
ggsave("Application/output/FIG7b_PSM_Rcam.pdf",h=8,w=8)
ggsave("Application/output/FIG7b_PSM_Rcam.eps",h=8,w=8)


cols <- c("tomato3","steelblue3","forestgreen")

p2 <- ggplot(data = A %>% as_tibble() %>% mutate(V4=factor(V4)) )+
  geom_line(aes(x=yseq,
                y=V1,
                col=V4,group=i), lwd = .8)+
  theme_bw()+
  scale_color_manual("DC",values = cols)+
  ylab("Density") + xlab("Birth weight")+
  theme(legend.position = "bottom",text=element_text(size=18),
        legend.key.width = unit(1,"cm"))
p2+facet_wrap(~"CAM")+ggview::canvas(h=8,w=8)

ggsave("Application/output/FIG9b_DENS_Cam.pdf",h=5,w=5)
ggsave("Application/output/FIG9b_DENS_Cam.eps",h=5,w=5)

sumCPP2 <- CPP2 %>% group_by(dcl) %>% summarise(med_logdde = median((dde)),
                                                med_loggest = median(
                                                  (gest)))

p3 <- ggplot(data = CPP2 %>% as_tibble())+
  geom_density(aes(x = (gest),
                   #group=hosp,
                   col=factor(dcl)), lwd = .8)+
  geom_vline(data=sumCPP2,aes(xintercept = med_loggest, col=factor(dcl)),lwd=1, lty=4)+
  theme_bw()+
  scale_color_manual("DC",values = cols)+
  ylab("Density") + xlab("Gest. time")+
  theme(legend.position = "none",text=element_text(size=18),
        legend.key.width = unit(1,"cm"))
p3



p4 <- ggplot(data = CPP2 %>% as_tibble())+
  geom_density(aes(
    x = (dde),
    #group=hosp,
    col=factor(dcl)), lwd = .8)+
  geom_vline(data=sumCPP2,aes(xintercept = med_logdde, col=factor(dcl)),lwd=1,lty=4)+
  theme_bw()+
  scale_color_manual("DC",values = cols)+
  xlab("DDE conc.") + ylab("Density")+
  theme(legend.position = "none",text=element_text(size=18),legend.key.width = unit(1,"cm"))

p4

p3+p4
