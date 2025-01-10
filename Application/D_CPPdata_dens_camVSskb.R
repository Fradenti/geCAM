source("Application/A_CPPdata_setup.R")
# Postprocessing -------------------------------------------------------------------------

Rskb <- readRDS("Application/RDS/run_CPP_SKPB.RDS")
Rcam <- readRDS("Application/RDS/run_CPP_CAM.RDS")


A = c()
for(i in 1:12){
  a = avg_dens(Rskb, howmany = 10000, s = i, yseq = seq(-25,150,by=0.5))
  A = rbind(A,cbind(a,i,clskb[i]))
}
B = c()
for(i in 1:12){
  b = avg_dens(Rcam, howmany = 10000, s = i, yseq = seq(-25,150,by=0.5))
  B = rbind(B,cbind(a,i,clcam[i]))
}

D1 = A %>% data.frame %>% mutate(mod="geCAM (SKBP)")
D2 = B %>% data.frame %>% mutate(mod="CAM")
D3 = rbind(D1,D2) %>% mutate(gr= factor(paste("Hospital",i)),
                             mod2 = factor(paste(mod,"- DC",V4) ))

D3$gr
levels(D3$gr) =  paste("Hospital",1:12)

D4 = data.frame(orig=Y, gr = factor(paste("Hospital",G)))
levels(D4$gr) =  paste("Hospital",1:12)
D4$gr

ggplot()+theme_bw()+
  geom_histogram(data=D4,
                 aes(x=Y,
                     y=after_stat(density)),
                 bins=15,col="gray", fill="lightgray",alpha=.1)+
  geom_line(data=D3,
            aes(x=yseq, y=V1, col=mod2, group=mod,lty=mod),lwd=.7)+
  facet_wrap(~gr)+guides(lty = "none")+
  ylab("Density")+xlab("Weight at birth")+
  scale_color_manual("",values = c("steelblue1","royalblue","darkblue", "darkred", "orange"))+
  theme(element_text(size=24),legend.position = "bottom")+
  ggview::canvas(h=6,w=8)
ggsave("Application/output/FIG8_estim_dens_hostpitals_skb_vs_cam.pdf",h=6,w=8)
ggsave("Application/output/FIG8_estim_dens_hostpitals_skb_vs_cam.eps",h=6,w=8,device = cairo_ps)





# monitoring K,L ----------------------------------------------------------


uniq_ES_CAM <- ts(apply(Rcam$ES,1,function(x) length(unique(x))))
uniq_ES_SKB <- ts(apply(Rskb$ES,1,function(x) length(unique(x))))
max_ES_CAM <- ts(apply(Rcam$ES,1,function(x) max(unique(x))))
max_ES_SKB <- ts(apply(Rskb$ES,1,function(x) max(unique(x))))
uniq_EM_CAM <- ts(apply(Rcam$EM,1,function(x) length(unique(x))))
uniq_EM_SKB <- ts(apply(Rskb$EM,1,function(x) length(unique(x))))
max_EM_CAM <- ts(apply(Rcam$EM,1,function(x) max(unique(x))))
max_EM_SKB <- ts(apply(Rskb$EM,1,function(x) max(unique(x))))

d1 = tibble(x = 1:10000, val = as.numeric(uniq_ES_CAM), model = "CAM", var = "# filled components", type = "DC" )
d2 = tibble(x = 1:10000, val = as.numeric(uniq_EM_CAM), model = "CAM", var = "# filled components", type = "OC" )
d3 = tibble(x = 1:10000, val = as.numeric(  max_ES_CAM), model = "CAM", var = "Max label value", type = "DC" )
d4 = tibble(x = 1:10000, val = as.numeric(  max_EM_CAM), model = "CAM", var = "Max label value", type = "OC" )
d5 = tibble(x = 1:10000, val = as.numeric(uniq_ES_SKB), model =  "geCAM (SKBP)", var = "# filled components", type = "DC" )
d6 = tibble(x = 1:10000, val = as.numeric(uniq_EM_SKB), model =  "geCAM (SKBP)", var = "# filled components", type = "OC" )
d7 = tibble(x = 1:10000, val = as.numeric(  max_ES_SKB), model = "geCAM (SKBP)", var = "Max label value", type = "DC" )
d8 = tibble(x = 1:10000, val = as.numeric(  max_EM_SKB), model = "geCAM (SKBP)", var = "Max label value", type = "OC" )

DD = bind_rows(d1,d2,d3,d4,d5,d6,d7,d8)


ggplot(DD)+ theme_bw()+
  #geom_hline(data=NULL,yintercept = c(30,50), lty=2, fill=c("steelblue3","tomato3"))+
  geom_line(aes(x=x, y=val, col=type),alpha=.9)+
  facet_grid(var ~model, scales="free")+
  scale_color_manual("",values = c("tomato3","steelblue3"))+
  theme(text=element_text(size=24))+
  xlab("Iteration")+ylab("")+
  theme(legend.position = "bottom")+ggview::canvas(h=8,w=15)
ggsave("Application/output/FIGS13_traces_thresh.pdf",h=8,w=15)
ggsave("Application/output/FIGS13_traces_thresh.eps",h=8,w=15,device = cairo_ps)
