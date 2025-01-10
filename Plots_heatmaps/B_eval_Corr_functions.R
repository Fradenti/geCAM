# functions for experiments -----------------------------------------------
CORR_DPdistr = function(lim_par1, # vector with limits for observational sb parameters
                        lim_par2, # vector with limits for observational sb parameters
                        par_dist, # distributional parameters - scalar, alpha DP
                        len = 50,
                        p = NULL, 
                        type = c("PY","2P","SK","DP")){
  # sig, theta or a,b
  
  type <- match.arg(type)
  P1   <-  seq(lim_par1[1], lim_par1[2], length.out=len)
  P2   <-  seq(lim_par2[1], lim_par2[2], length.out=len)
  
  MAT = matrix(NA,length(P1),length(P2))
  if(type == "PY"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - 
          (1 - q1_dp(a = par_dist)) * 
          (1 - q2_py(sig = P1[i], theta = P2[j]))
      }}
  }else if(type == "2P"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - 
          (1 - q1_dp(a = par_dist)) * 
          (1 - q2_2p(a = P1[i], b = P2[j]))
      }}
  }else if(type == "SK"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - 
          (1 - q1_dp(a = par_dist)) * 
          (1 - q2_skb(a = P1[i], b = P2[j], p = p))
      }}
  }else if(type=="DP"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - 
          (1 - q1_dp(a = P1[j])) * 
          (1 - q2_dp(b = P2[j]))
      }}
  }
  TW = cbind(as.data.frame(reshape2::melt(MAT)),
             par_dist[1],par_dist[2],
             tit = paste0(type,"(",par_dist[1],")"),
             p1 = rep(P1,len),
             p2 = rep(P2,rep(len,len)))
  return(list(m = MAT, tw = TW))
}


# functions for experiments -----------------------------------------------
CORR_DPobs = function(lim_par1, # vector with limits for observational sb parameters
                      lim_par2, # vector with limits for observational sb parameters
                      par_obs, # observational parameters - scalar, alpha DP
                      len = 50,
                      p = NULL, 
                      type = c("PY","2P","SK","DP")){
  # sig, theta or a,b
  
  type = match.arg(type)
  P1 = seq(lim_par1[1], lim_par1[2], length.out=len)
  P2 = seq(lim_par2[1], lim_par2[2], length.out=len)
  
  MAT = matrix(NA,length(P1),length(P2))
  if(type == "PY"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - ( 1 - q2_dp(b = par_obs)) * 
          ( 1 - q1_py(sig = P1[i], theta = P2[j]))
      }}
  }else if(type == "2P"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - (1 - q2_dp(b = par_obs)) * 
          (1 - q1_2p(a_d = P1[i], b_d = P2[j]))
      }}
  }else if(type == "SK"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - (1 - q2_dp(b = par_obs)) * 
          (1 - q1_skb(a_d = P1[i], b_d = P2[j],p = p))
      }}
  }else if(type=="DP"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = 1 - (1 - q1_dp(a = P1[j])) * 
          (1 - q2_dp(b = P2[j]))
      }}
  }
  TW = cbind(as.data.frame(reshape2::melt(MAT)),
             par_obs[1],par_obs[2],
             tit = paste0(type,"(",par_obs[1],")"),
             p1 = rep(P1,len),p2 = rep(P2,rep(len,len)))
  return(list(m = MAT, tw = TW))
}

# functions for experiments -----------------------------------------------
CORR = function(lim_par1, # vector with limits for observational sb parameters
                lim_par2, # vector with limits for observational sb parameters
                par_dist, # distributional parameters
                len = 50,
                p = NULL, 
                type = c("PY","2P","SK")){
  # sig, theta or a,b
  
  type = match.arg(type)
  P1 = seq(lim_par1[1], lim_par1[2], length.out=len)
  P2 = seq(lim_par2[1], lim_par2[2], length.out=len)
  
  MAT = matrix(NA,length(P1),length(P2))
  if(type == "PY"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = corr_PY(sig = P1[i],
                           theta = P2[j],
                           sig_d = par_dist[1],
                           theta_d = par_dist[2])
      }}
  }else if(type == "2P"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = corr_2p(a = P1[i], b = P2[j],a_d = par_dist[1],b_d = par_dist[2])
      }}
  }else if(type == "SK"){
    for(i in seq_along(P1)){
      for(j in seq_along(P2)){
        MAT[i,j] = corr_skb(a = P1[i], 
                            b = P2[j],
                            p = p,
                            a_d = par_dist[1],
                            b_d = par_dist[2])
      }}
    
  }
  TW = cbind(as.data.frame(reshape2::melt(MAT)),
             par_dist[1],par_dist[2],
             tit = paste0(type,"(",par_dist[1],",",par_dist[2],")"),
             p1 = rep(P1,len),p2 = rep(P2,rep(len,len)))
  return(list(m = MAT, tw = TW))
}
