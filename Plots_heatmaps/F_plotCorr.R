
source("Plots_heatmaps/A_q1q2_Corr_functions.R")
source("Plots_heatmaps/B_eval_Corr_functions.R")

# Experiments ------------------------------------------------------------------

# DP as Distributional Weight Process ------------------------------------------

## PY as observational ---------------------------------------------------------
dis_PY1_obs_PY = CORR(lim_par1 = c(.05,.99),
                      lim_par2 = c(0.05,2),
                      par_dist = c(0,1))
dis_PY2_obs_PY = CORR(lim_par1 = c(.05,.99),
                      lim_par2 = c(0.05,2),
                      par_dist = c(.5,1))
dis_PY3_obs_PY = CORR(lim_par1 = c(.05,.99),
                      lim_par2 = c(0.05,2),
                      par_dist = c(.9,1))
# first check

contour(dis_PY1_obs_PY$m)
contour(dis_PY2_obs_PY$m)
contour(dis_PY3_obs_PY$m)

## 2PBP as observational -------------------------------------------------------
dis_2PBP1_obs_2PBP = CORR(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(0.5,0.5),
                                type = "2P")
dis_2PBP2_obs_2PBP = CORR(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(1,1),
                                type = "2P")
dis_2PBP3_obs_2PBP = CORR(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(2,4),
                                type = "2P")
# first check
contour(dis_2PBP1_obs_2PBP$m)
contour(dis_2PBP2_obs_2PBP$m)
contour(dis_2PBP3_obs_2PBP$m)


## SKB as observational --------------------------------------------------------
dis_SKB1_obs_SKB1 = CORR(lim_par1 = c(.05,2),
                        lim_par2 = c(0.05,2),
                        par_dist  = c(0.5,0.5), 
                        p=.25,
                        type = "SK")
dis_SKB2_obs_SKB1 = CORR(lim_par1 = c(.05,2),
                         lim_par2 = c(0.05,2),
                         par_dist = c(1,1), 
                         p=.25,
                         type = "SK")
dis_SKB3_obs_SKB1 = CORR(lim_par1 = c(.05,2),
                         lim_par2 = c(0.05,2),
                         par_dist = c(2,4), 
                         p=.25,
                         type = "SK")
dis_SKB1_obs_SKB2 = CORR(lim_par1 = c(.05,2),
                         lim_par2 = c(0.05,2),
                         par_dist = c(0.5,0.5), 
                         p=.5,
                         type = "SK")
dis_SKB2_obs_SKB2 = CORR(lim_par1 = c(.05,2),
                         lim_par2 = c(0.05,2),
                         par_dist = c(1,1), 
                         p=.5,
                         type = "SK")
dis_SKB3_obs_SKB2 = CORR(lim_par1 = c(.05,2),
                         lim_par2 = c(0.05,2),
                         par_dist = c(2,4), 
                         p=.5,
                         type = "SK")
# first check
contour(dis_SKB1_obs_SKB1$m)
contour(dis_SKB2_obs_SKB1$m)
contour(dis_SKB3_obs_SKB1$m)
contour(dis_SKB1_obs_SKB2$m)
contour(dis_SKB2_obs_SKB2$m)
contour(dis_SKB3_obs_SKB2$m)

