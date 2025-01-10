# skip-break --------------------------------------------------------------
for(en in 1:3){
  for(gr in 1:3){
    cat(paste(en,gr))  
    name_load <- paste0("Simulation_Study_1/RUNS/SIM1skb_ngroups_",gr,"_samplesize_",en,".RDS")
    
    cat("\nLoading data\n")
    RESULTS1 <- readRDS(name_load)
    
    dens_list <- list()
    cat("Start loop\n")
    for(s in 1:ncol(RESULTS1[[1]][[1]]$ES)){
      Qsub = list()
      for(g in 1:length(RESULTS1)){
        cat(paste(g,""))
        Qsub[[g]] = extract_dens(RESULTS1[[g]],subject = s)
      }
      cat(paste("\n",s,"---------\n"))
      ind = paste0("Sub",s)
      dens_list[[ind]] = Qsub
    }
    saveRDS(dens_list,paste0("Simulation_Study_1/DENS/dens_list_skip_ngroups_",gr,"_samplesize_",en,"_sub_ALL.RDS"))
  }
}

# 2pbp --------------------------------------------------------------
for(en in 1:3){
  for(gr in 1:3){
    cat(paste(en,gr))  
    name_load <- paste0("Simulation_Study_1/RUNS/SIM2pbp_ngroups_",gr,"_samplesize_",en,".RDS")
    
    cat("\nLoading data\n")
    RESULTS1 <- readRDS(name_load)
    
    dens_list <- list()
    cat("Start loop\n")
    for(s in 1:ncol(RESULTS1[[1]][[1]]$ES)){
      Qsub = list()
      for(g in 1:length(RESULTS1)){
        cat(paste(g,""))
        Qsub[[g]] = extract_dens(RESULTS1[[g]],subject = s)
      }
      cat(paste("\n",s,"---------\n"))
      ind = paste0("Sub",s)
      dens_list[[ind]] = Qsub
    }
    saveRDS(dens_list,paste0("Simulation_Study_1/DENS/dens_list_2pbp_ngroups_",gr,"_samplesize_",en,"_sub_ALL.RDS"))
  }
}
