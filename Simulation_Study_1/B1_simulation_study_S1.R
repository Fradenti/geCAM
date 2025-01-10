
# Results -----------------------------------------------------------------
# extract density for single subject (first + last)


# skip-break --------------------------------------------------------------
for(en in 1:3){
  for(gr in 1:3){
  cat(paste(en,gr))  
    name_load <- paste0("Simulation_Study_1/RUNS/SIM1skb_ngroups_",gr,"_samplesize_",en,".RDS")
    
    RESULTS1 <- readRDS(name_load)

  dens_list1 <- list()
  dens_list2 <- list()
  for(g in 1:length(RESULTS1)){
    dens_list1[[g]] = extract_dens(RESULTS1[[g]])
    cat("halfway there!")
    dens_list2[[g]] = extract_dens(RESULTS1[[g]],subject = ncol(RESULTS1[[g]][[1]]$ES))
    cat(g)
  }
  saveRDS(dens_list1,paste0("Simulation_Study_1/DENS/dens_list_skip_ngroups_",gr,"_samplesize_",en,"_sub1.RDS"))
  saveRDS(dens_list2,paste0("Simulation_Study_1/DENS/dens_list_skip_ngroups_",gr,"_samplesize_",en,"_sub2.RDS"))
  }
  }





# gener Beta --------------------------------------------------------------

for(en in 1:3){
  for(gr in 1:3){
    cat(paste(en,gr))  
    name_load <- paste0("Simulation_Study_1/RUNS/SIM2pbp_ngroups_",gr,"_samplesize_",en,".RDS")
    
    RESULTS1 <- readRDS(name_load)
    
    dens_list1 <- list()
    dens_list2 <- list()
    for(g in 1:length(RESULTS1)){
      dens_list1[[g]] = extract_dens(RESULTS1[[g]])
      cat("halfway there!")
      dens_list2[[g]] = extract_dens(RESULTS1[[g]],subject = ncol(RESULTS1[[g]][[1]]$ES))
      cat(g)
    }
    saveRDS(dens_list1,paste0("Simulation_Study_1/DENS/dens_list_2pbp_ngroups_",gr,"_samplesize_",en,"_sub1.RDS"))
    saveRDS(dens_list2,paste0("Simulation_Study_1/DENS/dens_list_2pbp_ngroups_",gr,"_samplesize_",en,"_sub2.RDS"))
  }
}

