# GGPLOTS -----------------------------------------------------------------
library(latex2exp)

# sample process over the distr, different DP over observ

PYDPs <- rbind(
  dis_PY_obs_DP1$tw,
  dis_PY_obs_DP2$tw,
  dis_PY_obs_DP3$tw
)
PYDPs$tit = factor(PYDPs$tit)
levels(PYDPs$tit) =   c( `A` = TeX("$\\omega_k\\sim DP(0.5)$"),
                         `B` = TeX("$\\omega_k\\sim DP(1)$"),
                         `C` = TeX("$\\omega_k\\sim DP(2)$"))
breaks = seq(0, 1, by = 0.05)



PYDPs$row = 0
PYDPs$row = factor(PYDPs$row)
levels(PYDPs$row) =   c( `A` = TeX("$\\pi\\sim PYP(\\vartheta,\\sigma)$"))


ggplot(PYDPs)+
  geom_tile(aes(x=p1,y=p2,fill=value))+
  #  facet_wrap(~tit, labeller = label_parsed,ncol = )+
  facet_grid(row~tit, labeller = label_parsed)+
  scale_fill_viridis_c(option = "D")+
  metR::geom_contour2(aes(x=p1,y=p2,z=value),
                      #breaks = breaks,
                      col=1,bins = 15)+
  theme_bw()+
  metR::geom_text_contour(aes(x=p1,y=p2,z = value),
                          breaks = breaks,
                          stroke = 0.1)+
  theme(text = element_text(size=24),
        axis.text.x = element_text(size=13,
                                   angle = 90),
        axis.text.y = element_text(size=13),
        legend.position = "none"
  )+
  xlab(TeX("$\\sigma$"))+
  ylab(TeX("$\\vartheta$"))
#ggtitle(TeX("$\\pi\\sim PY(\\vartheta,\\sigma)$"))
ggview::ggview(h=5,w=15)
ggsave("Plots_heatmaps/REVISION_HEATMAPS/FIGS2a_Obs_DP_Dis_PY.pdf",h=5,w=15)
ggsave("Plots_heatmaps/REVISION_HEATMAPS/FIGS2a_Obs_DP_Dis_PY.eps",h=5,w=15)


###----------------------------------------------------------------
BeDPs <- rbind(
  dis_2PBP_obs_DP1$tw,
  dis_2PBP_obs_DP2$tw,
  dis_2PBP_obs_DP3$tw
)
BeDPs$tit = factor(BeDPs$tit)
levels(BeDPs$tit) =   c( `A` = TeX("$\\omega_k\\sim DP(0.5)$"),
                         `B` = TeX("$\\omega_k\\sim DP(1)$"),
                         `C` = TeX("$\\omega_k\\sim DP(2)$"))
breaks = seq(0, 1, by = 0.05)
BeDPs$row = 0
BeDPs$row = factor(BeDPs$row)
levels(BeDPs$row) =   c( `A` = TeX("$\\pi\\sim 2PBP(s_1,s_2)$"))


ggplot(BeDPs)+
  geom_tile(aes(x=p1,y=p2,fill=value))+
  #  facet_wrap(~tit, labeller = label_parsed,ncol = )+
  facet_grid(row~tit, labeller = label_parsed)+
  scale_fill_viridis_c(option = "D")+
  metR::geom_contour2(aes(x=p1,y=p2,z=value),
                      #breaks = breaks,
                      col=1,bins = 15)+
  theme_bw()+
  metR::geom_text_contour(aes(x=p1,y=p2,z = value),
                          breaks = breaks,
                          stroke = 0.1)+
  theme(text = element_text(size=24),
        axis.text.x = element_text(size=13,
                                   angle = 90),
        axis.text.y = element_text(size=13),
        legend.position = "none"
  )+
  xlab(TeX("$s_1$"))+
  ylab(TeX("$s_2$"))
ggview::ggview(h=5,w=15)
ggsave("Plots_heatmaps/REVISION_HEATMAPS/FIGS2b_Obs_DP_Dis_2PBP.pdf",h=5,w=15)
ggsave("Plots_heatmaps/REVISION_HEATMAPS/FIGS2b_Obs_DP_Dis_2PBP.eps",h=5,w=15)


###----------------------------------------------------------------
SKDPs1 <- rbind(
  dis_SKB1_obs_DP1$tw,
  dis_SKB1_obs_DP2$tw,
  dis_SKB1_obs_DP3$tw)
SKDPs2 <- rbind(
  dis_SKB2_obs_DP1$tw,
  dis_SKB2_obs_DP2$tw,
  dis_SKB2_obs_DP3$tw
)
SKDPs1$tit = factor(SKDPs1$tit)
SKDPs2$tit = factor(SKDPs2$tit)
levels(SKDPs1$tit) =   c(`A` = TeX("$\\omega_k\\sim DP(0.5)$"),
                         `B` = TeX("$\\omega_k\\sim DP(1)$"),
                         `C` = TeX("$\\omega_k\\sim DP(2)$"))
levels(SKDPs2$tit) =   c(`A` = TeX("$\\omega_k\\sim DP(0.5)$"),
                         `B` = TeX("$\\omega_k\\sim DP(1)$"),
                         `C` = TeX("$\\omega_k\\sim DP(2)$"))
breaks = seq(0, 1, by = 0.05)
SKDPs1$row = 0
SKDPs1$row = factor(SKDPs1$row)
levels(SKDPs1$row) =   c( `A` = TeX("$\\pi\\sim SKBP(s_1,s_2,0.25)$"))

SKDPs2$row = 0
SKDPs2$row = factor(SKDPs2$row)
levels(SKDPs2$row) =   c( `A` = TeX("$\\pi\\sim SKBP(s_1,s_2,0.5)$"))
SKDP <- rbind(SKDPs1,SKDPs2)

ggplot(SKDP)+
  geom_tile(aes(x=p1,y=p2,fill=value))+
  #  facet_wrap(~tit, labeller = label_parsed,ncol = )+
  facet_grid(row~tit, labeller = label_parsed)+
  scale_fill_viridis_c(option = "D")+
  metR::geom_contour2(aes(x=p1,y=p2,z=value),
                      #breaks = breaks,
                      col=1,bins = 15)+
  theme_bw()+
  metR::geom_text_contour(aes(x=p1,y=p2,z = value),
                          breaks = breaks,
                          stroke = 0.1)+
  theme(text = element_text(size=24),
        axis.text.x = element_text(size=13,
                                   angle = 90),
        axis.text.y = element_text(size=13),
        legend.position = "none"
  )+
  xlab(TeX("$s_1$"))+
  ylab(TeX("$s_2$"))

ggview::ggview(h=10,w=15)
ggsave("Plots_heatmaps/REVISION_HEATMAPS/FIGS3_Obs_DP_Dis_SKB.pdf",h=10,w=15)
ggsave("Plots_heatmaps/REVISION_HEATMAPS/FIGS3_Obs_DP_Dis_SKB.eps",h=10,w=15)

