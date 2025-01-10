library(tidyverse)

melted=data_skip %>% 
  reshape2::melt(c("truth","en","gr","sim","group","y", "pop"))

mm = melted %>% 
  group_by(truth, en, gr, pop, y, variable) %>% 
  reframe(md = mean(value))
c("tomato3","steelblue3")

melted %>% filter(en==1,gr==1) %>% 
  ggplot()+theme_bw()+
  geom_line(aes(x=y,y=value),col="gray",alpha=.3)+
  geom_line(aes(x=y,y=truth),col="tomato3")+
  geom_line(data=mm %>% filter(en==1,gr==1),
            aes(x=y,y=md),col="steelblue3")+
  facet_grid((variable~pop))
  
id.gr=2  
melted %>% filter(en==1,gr==id.gr) %>% 
  ggplot()+theme_bw()+
  geom_line(aes(x=y,y=value),col="gray",alpha=.3)+
  geom_line(aes(x=y,y=truth),col="tomato3")+
  geom_line(data=mm %>% filter(en==1,gr==id.gr),
            aes(x=y,y=md),col="steelblue3")+
  facet_grid((variable~pop))

id.gr=3  
melted %>% filter(en==1,gr==id.gr) %>% 
  ggplot()+theme_bw()+
  geom_line(aes(x=y,y=value),col="gray",alpha=.3)+
  geom_line(aes(x=y,y=truth),col="black")+
  geom_line(data=mm %>% filter(en==1,gr==id.gr),
            aes(x=y,y=md),col="blue")+
  facet_grid((variable~pop))


