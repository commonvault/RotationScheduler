


COVID_sim9 <- function(length_rotation,length_rotation_n,nspd,staggering=FALSE,input_params,nit=1000){
  #Setting Input parameters
  T <- input_params$T
  prob_medic_has_it_alrady <- input_params$prob_medic_has_it_already
  prob_patient_has_it_alrady <- input_params$prob_patient_has_it_already
  prob_doctor_patient_transmission <- input_params$prob_doctor_patient_transmission
  prob_patient_doctor_transmission <- input_params$prob_patient_doctor_transmission
  prob_patient_nurse_transmission <- input_params$prob_patient_nurse_transmission
  prob_nurse_patient_transmission <- input_params$prob_nurse_patient_transmission
  pmm <-  input_params$medic_medic_transmission_probability_factor
  days_out_doc <- input_params$days_out_doc
  prob_doc_critically_ill <- input_params$prob_doc_critically_ill
  prob_community_infection <- input_params$community_transmission_prob
  lognormal_param1 <- input_params$lognormal_param1
  lognormal_param2 <- input_params$lognormal_param2
  d_pat_inf <- input_params$days_past_symptom_patient_infective
  maxh_nurses <- input_params$maximum_hours_per_week_nurse
  n_patients_init <- input_params$npatients_init
  
  n_attendings <- input_params$n_attendings
  n_residents <- input_params$n_residents
  n_nurses <- input_params$n_nurses
  n_doctors <- as.integer(n_residents+n_attendings)
  ka <- input_params$n_attentings_at_once
  kr <- input_params$n_residents_at_once
  kn <- input_params$n_nurses_at_once
  
  gen_incub_period <- function(){
    return(exp(rnorm(n=1,mean = lognormal_param1,sd =  lognormal_param2 )))
  }
  
  p_geom <- 1/input_params$average_patient_stay
  d_asym <- -input_params$inc_minus_latent #At what covid status value to people become contagious
  
  
  #Scaling probability of nurse patient transmissions by hours present
  if(nspd==2){
    prob_patient_nurse_transmission <- 1-(1-prob_patient_nurse_transmission)^3
    prob_nurse_patient_transmission <- 1-(1-prob_nurse_patient_transmission)^3
  } else if(nspd==3){
    prob_patient_nurse_transmission <- 1-(1-prob_patient_nurse_transmission)^2
    prob_nurse_patient_transmission <- 1-(1-prob_nurse_patient_transmission)^2
  } else {
    print("Warning nurse shifts per day should be 2 OR 3")
  }
  
  #Running Simulations
  failures <- rep(NA,nit)
  failure_due_to_nurse_shortage <- rep(FALSE,nit)
  failure_due_to_resident_shortage <- rep(FALSE,nit)
  failure_due_to_attending_shortage <- rep(FALSE,nit)
  n_attendings_that_got_covid <- rep(NA,nit)
  n_residents_that_got_covid <- rep(NA,nit)
  n_nurses_that_got_covid <- rep(NA,nit)
  
  
  for(n in 1:nit){
    
    
    #Initializing Doctors Covid statuses
    doctor_time_onOff <- rep(0,n_doctors)
    doctor_already_infected <- rep(FALSE,n_doctors)
    doctors_COVID_status <- rep(-Inf,n_doctors)
    doctors_assignment <- c(rep("On",ka+kr),rep("Off",n_doctors-as.integer(ka+kr)))
    doctor_type <- c(rep("Attending",ka),rep("Resident",kr),rep("Attending",n_attendings-ka),rep("Resident",n_residents-kr))
    for(j in 1:n_doctors){
      if(runif(1)<prob_medic_has_it_alrady){
        doctors_COVID_status[j] <- runif(n=1,min = -gen_incub_period(),max = 0)-1
        doctor_already_infected[j] <- TRUE
      }
    }
    if(staggering){
      doctor_time_onOff <- sample(0:(length_rotation-1),size  = n_doctors,replace=TRUE)
    }

    
    
    
    #Initializing Nurses Covid statuses
    nurse_time_onOff <- rep(0,n_nurses)
    nurse_group <- rep(1:kn,ceiling(n_nurses/kn))
    nurse_group <- nurse_group[1:n_nurses]
    nurse_already_infected <- rep(FALSE,n_nurses)
    nurse_COVID_status <- rep(-Inf,n_nurses)
    nurse_assignment <- c(rep("On",kn*nspd),rep("Off",n_nurses-kn*nspd))
    for(j in 1:n_nurses){
      if(runif(1)<prob_medic_has_it_alrady){
        nurse_COVID_status[j] <- runif(n=1,min = -gen_incub_period(),max = 0)-1
        nurse_already_infected[j] <- TRUE
      }
    }
    nurses_past7 <- matrix(rep(0,7*n_nurses),nrow = n_nurses)
    
    
    ################  Initializing Present Patient statuses #####################################
    patient_already_infected <- rep(FALSE,n_patients_init)
    patient_COVID_status <- rep(-Inf,n_patients_init)
    patient_group <- rep(1:kn,ceiling(n_patients_init/kn))
    patient_group <- patient_group[1:n_patients_init]
    for(j in 1:n_patients_init){
      if(runif(1)<prob_patient_has_it_alrady){
        patient_COVID_status[j] <- runif(n=1,min = -gen_incub_period(),max = 0)-1
        patient_already_infected[j] <- TRUE
      }
    }
    
    t <- 0
    failure <- FALSE
    while(t <= T & failure==FALSE){
      t <- t+1

      #COVID cases evolve by 1 day
      doctors_COVID_status <- doctors_COVID_status+1
      nurse_COVID_status <- nurse_COVID_status+1
      patient_COVID_status <- patient_COVID_status+1
      
      
      ###############################################################################################
      ################  Simulating day of patient care exposures #####################################
      ################################################################################################
      n_patients_curr <- length(patient_COVID_status)
      
      for(k in 1:n_patients_curr){
        curr_nurses <- which(nurse_group==patient_group[k] & nurse_assignment=="On")
        curr_docs <- which(doctors_assignment=="On")
        
        for(j in 1:length(curr_docs)){
          
          curr_doc <- curr_docs[j] 
          #Doctor to patient transmission
          if(doctors_COVID_status[curr_doc]>d_asym & patient_already_infected[k]==FALSE){
            if(prob_doctor_patient_transmission > runif(1)){
              patient_already_infected[k] <- TRUE
              patient_COVID_status[k] <- -gen_incub_period()
            }
          }
          
          #Patient to doctor transmissions
          if( patient_COVID_status[k]> d_asym & patient_COVID_status[k] <d_pat_inf & doctor_already_infected[curr_doc]==FALSE){
            if(prob_patient_doctor_transmission > runif(1)){
              doctor_already_infected[curr_doc] <- TRUE
              doctors_COVID_status[curr_doc] <- -gen_incub_period()
            }
          }
        }
        
        for(j in 1:length(curr_nurses)){
          curr_nurse <- curr_nurses[j]
          #Nurse to patient transmission
          
          if(nurse_COVID_status[curr_nurse]>d_asym & patient_already_infected[k]==FALSE){
            if(prob_nurse_patient_transmission > runif(1)){
              patient_already_infected[k] <- TRUE
              patient_COVID_status[k] <-  -gen_incub_period()
            }
          }
          
          
          #Patient to nurse transmission
          if(patient_COVID_status[k]>d_asym & patient_COVID_status[k] <d_pat_inf & nurse_already_infected[curr_nurse]==FALSE){
            if(prob_patient_nurse_transmission > runif(1)){
              nurse_already_infected[curr_nurse] <- TRUE
              nurse_COVID_status[curr_nurse] <- -gen_incub_period()
            }
          }
        }
      }
      
      #############################################################################################      
      ############End of Patient Care exposures for Day ##########################################
      ###################################################################################################     
      
      n_infective_docs <- sum(doctors_COVID_status[curr_doc]>d_asym & doctors_assignment=="On")
      n_infective_nurses <- sum(nurse_COVID_status[curr_doc]>d_asym & nurse_assignment=="On")
      
      prob_transmission_doc <- 1-(1-pmm)^((n_infective_docs/2+n_infective_nurses/nspd)*3)
      
      if(nspd==2){
        prob_transmission_nurse <- 1-(1-pmm)^((n_infective_docs/2+n_infective_nurses/nspd)*3)
      } else if (nspd==3){
        prob_transmission_nurse <- 1-(1-pmm)^((n_infective_docs/2+n_infective_nurses/nspd)*2)
      }
      docs_on <- which(doctors_assignment=="On")
      for(j in 1:length(docs_on)){
        if(doctor_already_infected[docs_on[j]]==FALSE & runif(1)<prob_transmission_doc){
          doctor_already_infected[docs_on[j]] <- TRUE
          doctors_COVID_status[docs_on[j]] <- -gen_incub_period()
        }
      }
      
      nurses_on <- which(nurse_assignment=="On")
      for(j in 1:length(nurses_on)){
        if(nurse_already_infected[nurses_on[j]]==FALSE & runif(1)< prob_transmission_nurse){
          nurse_already_infected[nurses_on[j]] <- TRUE
          nurse_COVID_status[nurses_on[j]] <- -gen_incub_period()
        }
      }
      
      ################################################################################
      ###################### End of Doctor doctor transmisson ########################
      ################################################################################
      
      
      

      #Random Community Exposures For Doctors and Nurses
      for(j in 1:n_doctors){
        if(doctor_already_infected[j]==FALSE & runif(1)<prob_community_infection){
          doctor_already_infected[j] <- TRUE
          doctors_COVID_status[j] <- -gen_incub_period()
        }
      }
      
      for(j in 1:n_nurses){
        if(nurse_already_infected[j]==FALSE & runif(1)<prob_community_infection){
          nurse_already_infected[j] <- TRUE
          nurse_COVID_status[j] <- -gen_incub_period()
        }
      }
      
      #Releasing patinets 
      n_patients_curr <- length(patient_COVID_status)
      leave_idx <- runif(n_patients_curr)< p_geom
      patient_already_infected <- patient_already_infected[!leave_idx]
      patient_COVID_status <- patient_COVID_status[!leave_idx]
      patient_group <- patient_group[!leave_idx]
      
      #Adding new patients
      expected_out <- p_geom*n_patients_init
      prob <- expected_out-floor(expected_out)
      n_add <- sample(c(floor(expected_out),ceiling(expected_out)),size = 1,prob = c(1-prob,prob),replace=TRUE)
      
      
      for(j in 1:n_add){
        
        if(length(patient_group)>0){
          group_counts <- rep(NA,kn)
          for(l in 1:kn){
            group_counts[l] <- sum(patient_group==l)
          }
          group_new_patient <- which.min(group_counts)
        } else {
          group_new_patient <- 1
        }
        patient_group <- c(patient_group,group_new_patient)
        
        if(runif(1)<prob_patient_has_it_alrady){
          patient_COVID_status <- c(patient_COVID_status,runif(n=1,min = -gen_incub_period(),max = 0))
          patient_already_infected <- c(patient_already_infected,TRUE)
        } else {
          patient_COVID_status <- c(patient_COVID_status,-Inf)
          patient_already_infected <- c(patient_already_infected,FALSE)
        }
        
      }
      
      
      #Determining if doctor is critically ill/ can go back to work
      for(j in 1:n_doctors){
        if(round(doctors_COVID_status[j]) == days_out_doc-2){
          if(runif(1)<prob_doc_critically_ill){
            doctors_assignment[j] <- "Dead/TooIllToReturn"
            doctors_COVID_status[j] <- Inf
          } 
          
        }
      }
      
      
 
      for(j in 1:n_doctors){
        if(doctors_COVID_status[j] > days_out_doc & doctors_COVID_status[j] <Inf){
          doctors_COVID_status[j] <- -Inf
        }
      }
      
      #Determining if nurses are critically ill/can go back to work
      for(j in 1:n_nurses){
        if(round(nurse_COVID_status[j]) == days_out_doc-2){
          if(runif(1)<prob_doc_critically_ill){
            nurse_assignment[j] <- "Dead/TooIllToReturn"
            nurse_COVID_status[j] <- Inf
          } 
          
        }
      }
      
      for(j in 1:n_nurses){
        if(nurse_COVID_status[j] > days_out_doc & nurse_COVID_status[j]<Inf){
          nurse_COVID_status[j] <- -Inf
        }
      }
      
      #Rotation Days increase by 1
      temp <- rep(0,n_nurses)
      temp[nurse_assignment=="On"] <- 24/nspd
      nurses_past7 <- cbind(nurses_past7[,2:7],temp)
      nurse_time_onOff <- nurse_time_onOff+1
      doctor_time_onOff <- doctor_time_onOff+1
      
      ##############################################################     
      ######    #SWAPPING OUT MEDICAL STAFF WHO ARE SICK################
      ##################################################################   
      
      #Reassigning Attendings Based on COVID STATUS
      remove_doc <- doctors_COVID_status > 0 & doctors_assignment != "Dead/TooIllToReturn" & doctor_type == "Attending"
      doctor_time_onOff[remove_doc & doctors_assignment=="On"] <- 0
      doctors_assignment[remove_doc] <- "Off"
      avail_attendings <- doctors_COVID_status <= 0 & doctor_type == "Attending"
      
      
      if(sum(avail_attendings) < ka){
        failure=TRUE
        failure_due_to_attending_shortage[n] <- TRUE
      } else if(sum(doctors_assignment=="On" & doctor_type == "Attending") < ka) {
        replacements_needed <- ka-sum(doctors_assignment=="On" & doctor_type == "Attending")
        #Replace with avaliable residents who has have been off for the longest period of time
        for(j in 1:replacements_needed){
          durations_rest_avail <- doctor_time_onOff[doctors_assignment=="Off" & avail_attendings]
          idx_on <- which(avail_attendings & doctors_assignment=="Off" & doctor_time_onOff==max(durations_rest_avail))[1]
          doctor_time_onOff[idx_on] <- 0
          doctors_assignment[idx_on] <- "On"
        }
      }
      
      
      #Reassigning Residents Based on COVID STATUS
      remove_doc <- doctors_COVID_status > 0 & doctors_assignment != "Dead/TooIllToReturn" & doctor_type == "Resident"
      doctor_time_onOff[remove_doc & doctors_assignment=="On"] <- 0
      doctors_assignment[remove_doc] <- "Off"
      avail_residents <- doctors_COVID_status <= 0 & doctor_type == "Resident"
      
      
      
      if(sum(avail_residents) < kr){
        failure=TRUE
        failure_due_to_resident_shortage[n] <- TRUE
      } else if(sum(doctors_assignment=="On" & doctor_type == "Resident") < kr) {
        replacements_needed <- kr-sum(doctors_assignment=="On" & doctor_type == "Resident")
        #Replace with avaliable residents who has have been off for the longest period of time
        for(j in 1:replacements_needed){
          durations_rest_avail <- doctor_time_onOff[doctors_assignment=="Off" & avail_residents]
          idx_on <- which(avail_residents & doctors_assignment=="Off" & doctor_time_onOff==max(durations_rest_avail))[1]
          doctor_time_onOff[idx_on] <- 0
          doctors_assignment[idx_on] <- "On"
        }
      }
      
      
      
      #Reassigning Nurses Based on COVID STATUS
      remove_nurse <- (nurse_COVID_status > 0  | rowSums(nurses_past7) >= maxh_nurses) & nurse_assignment != "Dead/TooIllToReturn" 
      nurse_time_onOff[remove_nurse & nurse_assignment=="On"] <- 0
      nurse_assignment[remove_nurse] <- "Off"
      avail_nurses <- nurse_COVID_status <= 0 | rowSums(nurses_past7)>= maxh_nurses
      
      groups_sizes <- rep(NA,kn)
      for(l in 1:kn){
        groups_sizes[l] <- sum(nurse_group==l & nurse_assignment=="On")
      }
    
      
      if(sum(avail_nurses)< kn *nspd){
        failure=TRUE
        failure_due_to_nurse_shortage[n] <- TRUE
      } else{
        for(l in 1:kn){
          if(groups_sizes[l] < nspd){
            #Replace with avaliable nurse who has been off for the longest period of time
            replacements_needed <- nspd-groups_sizes[l]
            if(replacements_needed>0){
            for(j in 1:replacements_needed){
              durations_rest_avail <- nurse_time_onOff[nurse_assignment=="Off" & avail_nurses]
              idx_on <- which(avail_nurses & nurse_assignment=="Off" & nurse_time_onOff==max(durations_rest_avail))[1]
              nurse_time_onOff[idx_on] <- 0
              nurse_assignment[idx_on] <- "On"
              nurse_group[idx_on] <- l
            }
            }
          }
        }
      }
      
      
      #FULLFILLING ROTATIONS ON APPROPRIATE DAYS  
      #Fulfilling attending and Resdident Rotation if possible
    
      
      if(staggering==FALSE){
        attendings_on_idx <- which(doctors_assignment=="On" & doctor_type == "Attending")
        if(!failure & t %% length_rotation == 0){
          if(sum((doctors_assignment=="Off") & avail_attendings)>0){
            available_replacements <- sum((doctors_assignment=="Off") & avail_attendings)
            for(j in 1:min(c(available_replacements,ka))){
              durations_rest_avail <- doctor_time_onOff[(doctors_assignment=="Off") & avail_attendings]
              idx_on <- which(avail_attendings & (doctors_assignment=="Off") & (doctor_time_onOff==max(durations_rest_avail)))[1]
              doctor_time_onOff[idx_on] <- 0
              doctors_assignment[idx_on] <- "On"
              doctor_time_onOff[attendings_on_idx[j]] <- 0
              doctors_assignment[attendings_on_idx[j]] <- "Off"
          }
        }
      }
      
      #Fulfilling resident rotation if possible
      residents_on_idx <- which(doctors_assignment=="On" & doctor_type == "Resident")
      if(!failure & t %% length_rotation == 0){
        if(sum((doctors_assignment=="Off") & avail_residents)>0){
          available_replacements <- sum((doctors_assignment=="Off") & avail_residents)
          for(j in 1:min(c(available_replacements,kr))){
            durations_rest_avail <- doctor_time_onOff[(doctors_assignment=="Off") & avail_residents]
            idx_on <- which(avail_residents & (doctors_assignment=="Off") & (doctor_time_onOff==max(durations_rest_avail)))[1]
            doctor_time_onOff[idx_on] <- 0
            doctors_assignment[idx_on] <- "On"
            doctor_time_onOff[residents_on_idx[j]] <- 0
            doctors_assignment[residents_on_idx[j]] <- "Off"
            }
          }
        }
      } else if(!failure & staggering==TRUE){
        attendings_to_replace <- which(doctors_assignment=="On" & doctor_time_onOff >= length_rotation & doctor_type=="Attending")
        available_replacements <- sum((doctors_assignment=="Off") & avail_attendings)
        if(length(attendings_to_replace)>0 & available_replacements>0){
          for(j in 1:min(c(available_replacements,length(attendings_to_replace)))){
            durations_rest_avail <- doctor_time_onOff[(doctors_assignment=="Off") & avail_attendings]
            idx_on <- which(avail_attendings & (doctors_assignment=="Off") & (doctor_time_onOff==max(durations_rest_avail)))[1]
            doctor_time_onOff[idx_on] <- 0
            doctors_assignment[idx_on] <- "On"
            doctor_time_onOff[attendings_to_replace[j]] <- 0
            doctors_assignment[attendings_to_replace[j]] <- "Off"
          }
        }
        
        residents_to_replace <- which(doctors_assignment=="On" & doctor_time_onOff >= length_rotation & doctor_type=="Resident")
        available_replacements <- sum((doctors_assignment=="Off") & avail_residents)
        if(length(residents_to_replace)>0 & available_replacements>0){
          for(j in 1:min(c(available_replacements,length(residents_to_replace)))){
            durations_rest_avail <- doctor_time_onOff[(doctors_assignment=="Off") & avail_residents]
            idx_on <- which(avail_residents & (doctors_assignment=="Off") & (doctor_time_onOff==max(durations_rest_avail)))[1]
            doctor_time_onOff[idx_on] <- 0
            doctors_assignment[idx_on] <- "On"
            doctor_time_onOff[residents_to_replace[j]] <- 0
            doctors_assignment[residents_to_replace[j]] <- "Off"
          }
        }
        
        
        
        
      }
      
      #Fulfilling Nurse Rotations if possible
      if(!failure & t %% length_rotation_n == 0 & staggering==FALSE){
        for(g in 1:kn){
          nurse_on_idx <- which(nurse_assignment=="On" & nurse_group==g)
          if(sum(nurse_assignment=="Off" & avail_nurses)>0){
            available_replacements <- sum(nurse_assignment=="Off" & avail_nurses)
            for(j in 1:min(c(available_replacements,nspd))){
              durations_rest_avail <- nurse_time_onOff[nurse_assignment=="Off" & avail_nurses]
              idx_on <- which(avail_nurses & nurse_assignment=="Off" & nurse_time_onOff==max(durations_rest_avail))[1]
              nurse_time_onOff[idx_on] <- 0
              nurse_assignment[idx_on] <- "On"
              nurse_time_onOff[nurse_on_idx[j]] <- 0
              nurse_assignment[nurse_on_idx[j]] <- "Off"
              nurse_group[idx_on] <- g
            }
          }
          
        }
      } else if(!failure & staggering==TRUE) {
          nurse_to_replace_idx <- which(nurse_assignment=="On"  & nurse_time_onOff >= length_rotation_n)
          available_replacements <- sum(nurse_assignment=="Off" & avail_nurses )
          if(available_replacements>0 & length(nurse_to_replace_idx)>0){
            for(j in 1:min(c(available_replacements,length(nurse_to_replace_idx)))){
              durations_rest_avail <- nurse_time_onOff[nurse_assignment=="Off" & avail_nurses ]
              idx_on <- which(avail_nurses & nurse_assignment=="Off" & nurse_time_onOff==max(durations_rest_avail))[1]
              nurse_group[idx_on] <- nurse_group[nurse_to_replace_idx[j]]
              nurse_time_onOff[idx_on] <- 0
              nurse_assignment[idx_on] <- "On"
              nurse_time_onOff[nurse_to_replace_idx[j]] <- 0
              nurse_assignment[nurse_to_replace_idx[j]] <- "Off"
            }
          }
          
        }
        
        
        
        
      
      
      
    }
    
    #Storing metrics from Monte Carlo Simulation
    failures[n] <-failure
    n_attendings_that_got_covid[n] <- sum(doctor_already_infected & doctor_type=="Attending")
    n_residents_that_got_covid[n] <- sum(doctor_already_infected & doctor_type=="Resident")
    n_nurses_that_got_covid[n] <- sum(nurse_already_infected)

  }
  #}
  
  print(medic_medic_transmissions/(medic_medic_transmissions+medic_patient_transmissions))
  return(list(failures=failures,ac=n_attendings_that_got_covid,rc=n_residents_that_got_covid,nc=n_nurses_that_got_covid,
              nf=failure_due_to_nurse_shortage, rf =failure_due_to_resident_shortage, af = failure_due_to_attending_shortage ))
}