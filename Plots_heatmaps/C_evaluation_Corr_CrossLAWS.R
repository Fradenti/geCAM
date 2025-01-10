
source("Plots_heatmaps/A_q1q2_Corr_functions.R")
source("Plots_heatmaps/B_eval_Corr_functions.R")

# Experiments ------------------------------------------------------------------

# DP as Distributional Weight Process ------------------------------------------

## PY as observational ---------------------------------------------------------
dis_DP1_obs_PY = CORR_DPdistr(lim_par1 = c(.05,.99),
                      lim_par2 = c(0.05,2),
                      par_dist = c(0.5))
dis_DP2_obs_PY = CORR_DPdistr(lim_par1 = c(.05,.99),
                      lim_par2 = c(0.05,2),
                      par_dist = c(1))
dis_DP3_obs_PY = CORR_DPdistr(lim_par1 = c(.05,.99),
                      lim_par2 = c(0.05,2),
                      par_dist = c(2))
# first check

contour(dis_DP1_obs_PY$m)
contour(dis_DP2_obs_PY$m)
contour(dis_DP3_obs_PY$m)

## 2PBP as observational -------------------------------------------------------
dis_DP1_obs_2PBP = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(0.5),
                                type = "2P")
dis_DP2_obs_2PBP = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(1),
                                type = "2P")
dis_DP3_obs_2PBP = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(2),
                                type = "2P")
# first check
contour(dis_DP1_obs_2PBP$m)
contour(dis_DP2_obs_2PBP$m)
contour(dis_DP3_obs_2PBP$m)


## SKB as observational --------------------------------------------------------
dis_DP1_obs_SKB1 = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(0.5), p=.5,
                                type = "SK")
dis_DP2_obs_SKB1 = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(1), p=.5,
                                type = "SK")
dis_DP3_obs_SKB1 = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(2), p=.5,
                                type = "SK")
dis_DP1_obs_SKB2 = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(0.5), p=.25,
                                type = "SK")
dis_DP2_obs_SKB2 = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(1), p=.25,
                                type = "SK")
dis_DP3_obs_SKB2 = CORR_DPdistr(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_dist = c(2), p=.25,
                                type = "SK")
# first check
contour(dis_DP1_obs_SKB1$m)
contour(dis_DP2_obs_SKB1$m)
contour(dis_DP3_obs_SKB1$m)
contour(dis_DP1_obs_SKB2$m)
contour(dis_DP2_obs_SKB2$m)
contour(dis_DP3_obs_SKB2$m)

# DP as Observational Weight Process -------------------------------------------

## PY as distributional ---------------------------------------------------------
dis_PY_obs_DP1 = CORR_DPobs(lim_par1 = c(.05,.99),
                            lim_par2 = c(0.05,2),
                            par_obs = c(0.5))
dis_PY_obs_DP2 = CORR_DPobs(lim_par1 = c(.05,.99),
                            lim_par2 = c(0.05,2),
                            par_obs = c(1))
dis_PY_obs_DP3 = CORR_DPobs(lim_par1 = c(.05,.99),
                            lim_par2 = c(0.05,2),
                            par_obs = c(2))
# first check

contour(dis_PY_obs_DP1$m)
contour(dis_PY_obs_DP2$m)
contour(dis_PY_obs_DP3$m)

## 2PBP as distributional ------------------------------------------------------
dis_2PBP_obs_DP1 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(0.5),
                              type = "2P")
dis_2PBP_obs_DP2 = CORR_DPobs(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_obs = c(1),
                                type = "2P")
dis_2PBP_obs_DP3 = CORR_DPobs(lim_par1 = c(.05,2),
                                lim_par2 = c(0.05,2),
                                par_obs = c(2),
                                type = "2P")
# first check

contour(dis_2PBP_obs_DP1$m)
contour(dis_2PBP_obs_DP2$m)
contour(dis_2PBP_obs_DP3$m)


## SKB as distributional ------------------------------------------------------
dis_SKB1_obs_DP1 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(0.5), p=.5,
                              type = "SK")
dis_SKB1_obs_DP2 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(1), p=.5,
                              type = "SK")
dis_SKB1_obs_DP3 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(2), p=.5,
                              type = "SK")
dis_SKB2_obs_DP1 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(0.5), p=.25,
                              type = "SK")
dis_SKB2_obs_DP2 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(1), p=.25,
                              type = "SK")
dis_SKB2_obs_DP3 = CORR_DPobs(lim_par1 = c(.05,2),
                              lim_par2 = c(0.05,2),
                              par_obs = c(2), p=.25,
                              type = "SK")
# first check

contour(dis_SKB1_obs_DP1$m)
contour(dis_SKB1_obs_DP2$m)
contour(dis_SKB1_obs_DP3$m)
contour(dis_SKB2_obs_DP1$m)
contour(dis_SKB2_obs_DP2$m)
contour(dis_SKB2_obs_DP3$m)









