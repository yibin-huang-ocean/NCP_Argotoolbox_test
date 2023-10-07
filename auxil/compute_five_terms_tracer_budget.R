

# 1. Tracer change; 2. vertical mixing 3. ekman pumping 4, Entrainment; 5. background tracer change
compute_five_terms_tracer_budget <- function(  
    cycle,
    date,
    longitude,
    longitude_median_PS,
    latitude,
    latitude_median_PS,
    temperature,
    temperature_background,
    temperature_background_reference_location,
    salinity ,
    salinity_background ,
    salinity_background_reference_location,
    tracer,
    tracer_background,
    tracer_background_reference_location,
    pressure,
    Kz,
    MLD,
    ekman_v,
    season_label,
    #  MLD_defination= Model_setting_list$MLD_defination,
    # integration_depth_error= integration_depth_error,
    smooth_number ,
    integration_depth,
    EP_term_computation,
    background_correction)   {
  
  # Merge the input data 
  float_profile_merge <- data.frame(
  cycle=cycle,
  date=date,
  longitude=longitude,
  longitude_median_PS,
  latitude=latitude,
  latitude_median_PS,
  temperature=temperature,
  temperature_background= temperature_background,
  temperature_background_reference_location=temperature_background_reference_location,
  salinity=salinity ,
  salinity_background=salinity_background,
  salinity_background_reference_location=salinity_background_reference_location,
  tracer=tracer,
  tracer_background=tracer_background,
  tracer_background_reference_location=tracer_background_reference_location,
  pressure=pressure,
  Kz=Kz,
  MLD=MLD,
  ekman_v=ekman_v,
  season_label=season_label,

  smooth_number=smooth_number ,
  integration_depth=integration_depth,
  EP_term_computation=EP_term_computation,
  background_correction=background_correction) 


  float_profile_merge =arrange(  float_profile_merge ,  
                                 float_profile_merge$cycle,  
                                 float_profile_merge$pressure)
  
  

# Convert the tracer into the mmol m-3 ------------------------------------

  float_profile_merge$tracer <- sw_dens(  float_profile_merge$salinity[!is.na(float_profile_merge$salinity)],
                                          float_profile_merge$temperature[!is.na(float_profile_merge$salinity)])/1000 *float_profile_merge$tracer[!is.na(float_profile_merge$salinity)]
  
  
  float_profile_merge$tracer_background[!is.na(float_profile_merge$salinity_background)] <- sw_dens(  float_profile_merge$salinity_background[!is.na(float_profile_merge$salinity_background)],
                                          float_profile_merge$temperature_background[!is.na(float_profile_merge$salinity_background)])/1000 *float_profile_merge$tracer_background[!is.na(float_profile_merge$salinity_background)]
  
  float_profile_merge$tracer_background_reference_location[!is.na(float_profile_merge$salinity_background_reference_location)] <- sw_dens(  float_profile_merge$salinity_background_reference_location[!is.na(float_profile_merge$salinity_background_reference_location)],
                                                     float_profile_merge$temperature_background_reference_location[!is.na(float_profile_merge$salinity_background_reference_location)])/1000 *float_profile_merge$tracer_background_reference_location[!is.na(float_profile_merge$salinity_background_reference_location)]
 
  
  # Extract each cycle 
   cycle_data=  subset( float_profile_merge,
                        float_profile_merge$pressure==1)
  
  # Step 2: Compute the temporal change in the MLD between two cycles  (m d-1)
  cycle_data$MLD_change <- 0
  cycle_data$MLD_change_rate<- 0
  
  cycle_data$MLD_change[1:(length(    cycle_data$cycle)-1)] <-     cycle_data$MLD[2:length(    cycle_data$cycle)]-  
    cycle_data$MLD[1:(length(    cycle_data$cycle)-1)]
  
  cycle_data$MLD_change= moving_smooth_fill(  cycle_data$MLD_change, 
                                              smooth_number  )
  
  cycle_data$MLD_change_rate[1:(length(    cycle_data$cycle)-1)] <-     cycle_data$MLD_change[1:(length(    cycle_data$cycle)-1)]/ (   cycle_data$date[2:length(    cycle_data$cycle)]-  
                                                                                                                                cycle_data$date[1:(length(    cycle_data$cycle)-1)])
  
  cycle_data$MLD_change[     cycle_data$MLD_change<0]=0 # set the negative value of time rate of change in MLD to zero
  cycle_data$MLD_change_rate[     cycle_data$MLD_change_rate<0]=0 # set the negative value of time rate of change in MLD to zero
  
  
  MLD_change_rate= data.frame(cycle= cycle_data$cycle,
                         MLD_change_rate=cycle_data$MLD_change_rate,
                         MLD_change=cycle_data$MLD_change)
  float_profile_merge=left_join( float_profile_merge,     MLD_change_rate, "cycle")

  float_profile_merge =arrange(  float_profile_merge ,  
                                 float_profile_merge$cycle,  
                                 float_profile_merge$pressure)
  
 
  # Loop to compute different abiotic terms for each cycle 
  i=1
  cycle_data$tracer_mean_concentration=NaN
  for (i in 1:length(       cycle_data$cycle)){
    
    per_cycle_data= subset(  float_profile_merge,
                         float_profile_merge$cycle==       cycle_data$cycle[i])
    
    per_cycle_data=arrange(     per_cycle_data,      per_cycle_data$pressure)
    
    
    # Check if the deepest sampling depth is greater than the integration depth
    
    if ( max(per_cycle_data$pressure)+8 > per_cycle_data$integration_depth[1]   ){
      
      # tracer mean concentration with the salinity normalization
      
      if (    EP_term_computation==1 ){  # salinity normalization 
        salinity_mean =  mean(float_profile_merge$salinity[float_profile_merge$pressure<200],na.rm=T)
        cycle_data$tracer_mean_concentration[i] <-  mean (    per_cycle_data$tracer[1:    per_cycle_data$integration_depth[1]] /per_cycle_data$salinity[1:    per_cycle_data$integration_depth[1]]     )*   salinity_mean
        
        if (background_correction==3){
          cycle_data$tracer_background_mean_concentration[i] <-  mean (    per_cycle_data$tracer_background[1:    per_cycle_data$integration_depth[1]]/ per_cycle_data$salinity_background[1:    per_cycle_data$integration_depth[1]])*   salinity_mean
          cycle_data$tracer_background_mean_concentration_reference_location[i] <-  mean (    per_cycle_data$tracer_background_reference_location[1:    per_cycle_data$integration_depth[1]]/ per_cycle_data$salinity_background_reference_location[1:    per_cycle_data$integration_depth[1]])*   salinity_mean
          
        }   # Bracket for " if (background_correction==3)"
        
        if (background_correction==2){
          cycle_data$tracer_background_mean_concentration[i] <-  mean (    per_cycle_data$tracer_background[1:    per_cycle_data$integration_depth[1]]/ per_cycle_data$salinity_background[1:    per_cycle_data$integration_depth[1]])*   salinity_mean
          cycle_data$tracer_background_mean_concentration_reference_location[i] <- 0
          
        }  # Bracket for " if (background_correction==2)"
        
        if (background_correction==1){
          cycle_data$tracer_background_mean_concentration[i] <- 0
          cycle_data$tracer_background_mean_concentration_reference_location[i] <- 0
          
        }  # Bracket for " if (background_correction==2)"
        
      } # Bracket for " if (    EP_term_computation==1 )"
      
      
      if ( EP_term_computation==0 | EP_term_computation==2){ # not apply the salinity normalization
        cycle_data$tracer_mean_concentration[i] <-  mean (    per_cycle_data$tracer[1:    per_cycle_data$integration_depth[1]])
        cycle_data$tracer_background_mean_concentration[i] <-  mean (    per_cycle_data$tracer_background[1:    per_cycle_data$integration_depth[1]])
        cycle_data$tracer_background_mean_concentration_reference_location[i] <-  mean (    per_cycle_data$tracer_background_reference_location[1:    per_cycle_data$integration_depth[1]])
        
      }
      
      
      # Vertical diffusion term 
      line_integration_depth=which(per_cycle_data$pressure== per_cycle_data$integration_depth[1]) 
      if (    EP_term_computation==1 ){ # salinity normalization 
        
        salinity_mean =  mean(float_profile_merge$salinity[float_profile_merge$pressure<200],na.rm=T)
        
        tracer_gradient_diffusion <-  salinity_mean *
          ( mean (   per_cycle_data$tracer[ line_integration_depth:(line_integration_depth+5 )]/per_cycle_data$salinity[ line_integration_depth:(line_integration_depth+5 )])- 
              mean ( per_cycle_data$tracer[    (line_integration_depth-5): line_integration_depth]/per_cycle_data$salinity[    (line_integration_depth-5): line_integration_depth]) )/5
        cycle_data$vertical_diffusion[i]=  3600*24*  per_cycle_data$Kz[  line_integration_depth] *   tracer_gradient_diffusion
         
      } else{
        tracer_gradient_diffusion<-    ( mean (   per_cycle_data$tracer[ line_integration_depth:(line_integration_depth+5 )])- mean ( per_cycle_data$tracer[    (line_integration_depth-5): line_integration_depth]) )/5
        cycle_data$vertical_diffusion[i]<-  3600*24*  per_cycle_data$Kz[  line_integration_depth] * tracer_gradient_diffusion
     
        
      } # Bracket for "if (    EP_term_computation==1 ){"
   
      
      
      # Entrainment term
      if (    EP_term_computation==1 ){ # salinity normalization 
        
        salinity_mean =  mean(float_profile_merge$salinity[float_profile_merge$pressure<200],na.rm=T)
        tracer_c_MLD <-  mean (    per_cycle_data$tracer[1:    per_cycle_data$MLD[1]]/ per_cycle_data$salinity[1:    per_cycle_data$MLD[1]])*     salinity_mean
        tracer_c_MLD_base <-  mean (    per_cycle_data$tracer [ per_cycle_data$MLD[1]:    ( per_cycle_data$MLD[1]+per_cycle_data$MLD_change[1])]/
                                                        per_cycle_data$salinity [ per_cycle_data$MLD[1]:    ( per_cycle_data$MLD[1]+per_cycle_data$MLD_change[1])]) *     salinity_mean
         
      } else{
        
         tracer_c_MLD <-  mean (    per_cycle_data$tracer[1:    per_cycle_data$MLD[1]])
         tracer_c_MLD_base <-  mean (    per_cycle_data$tracer[ per_cycle_data$MLD[1]:    ( per_cycle_data$MLD[1]+per_cycle_data$MLD_change[1])])
      }
     
      cycle_data$entrainment[i] <-    cycle_data$MLD_change_rate[i]* (tracer_c_MLD_base-
                                                                   tracer_c_MLD) # mmol m-2 d-1
      
      
      
      # Vertical advection
      
      if (    EP_term_computation==1 ){ # salinity normalization 
        salinity_mean =  mean(float_profile_merge$salinity[float_profile_merge$pressure<200],na.rm=T)
        tracer_gradient_ekman <- sum (  derive_1( per_cycle_data$pressure,per_cycle_data$tracer/per_cycle_data$salinity*salinity_mean)[1: per_cycle_data$integration_depth[1]] )
      } else{
        tracer_gradient_ekman <- sum (  derive_1( per_cycle_data$pressure,per_cycle_data$tracer)[1: per_cycle_data$integration_depth[1]] )
      }
    
      cycle_data$vertical_advection[i]<- per_cycle_data$ekman_v[ 1] *   tracer_gradient_ekman
     
       # Add the seasonal label
      cycle_data$season_label[i]= per_cycle_data$season_label[1]
   
    }    # bracket for  " if ( max(per_cycle_data$pressure)+8 > per_cycle_data$integration_depth[1]   ){"                 
  
    
    
  }    # bracket for   " for (i in 1:length(  cycle_MLD$cycle))"  
  

  # Compute the time rate of tracer inventory change 

  cycle_data$tracer_mean_concentration <- moving_smooth(    cycle_data$tracer_mean_concentration,
                                                                smooth_number )
  
  
  cycle_data$tracer_inventory_change_rate=NaN
  tracer_inventory_change= (  cycle_data$tracer_mean_concentration[2:length(    cycle_data$cycle)]-  
                              cycle_data$tracer_mean_concentration[1:(length(    cycle_data$cycle)-1)]) *
    (   cycle_data$integration_depth[2:length(    cycle_data$cycle)]+  cycle_data$integration_depth[1:(length(    cycle_data$cycle)-1)])/2
  
  cycle_data$tracer_inventory_change_rate[1:(length(    cycle_data$cycle)-1)] <-  tracer_inventory_change / (   cycle_data$date[2:length(  cycle_data$cycle)]-   cycle_data$date[1:(length(   cycle_data$cycle)-1)])
  
  # Compute the time rate of background tracer inventory change 

  cycle_data$tracer_background_mean_concentration<-  cycle_data$tracer_background_mean_concentration-  cycle_data$tracer_background_mean_concentration_reference_location
  
  if (background_correction==3){
    cycle_data$tracer_background_mean_concentration <- moving_smooth_label (data=cycle_data$tracer_background_mean_concentration,  
                                                                            
                                                                            label= year_North_South( cycle_data$latitude,  cycle_data$date), 
                                                                            smooth_number= smooth_number )$data
  } else{
    cycle_data$tracer_background_mean_concentration <- moving_smooth (data=cycle_data$tracer_background_mean_concentration,  
                                                         
                                                                            smooth_number= smooth_number )
  }
 
  cycle_data$tracer_background_inventory_change_rate=0 
  tracer_background_inventory_change= (  cycle_data$tracer_background_mean_concentration[2:length(    cycle_data$cycle)]-  
                                         cycle_data$tracer_background_mean_concentration[1:(length(    cycle_data$cycle)-1)]) *
    (   cycle_data$integration_depth[2:length(    cycle_data$cycle)]+  cycle_data$integration_depth[1:(length(    cycle_data$cycle)-1)])/2
  
  cycle_data$tracer_background_inventory_change_rate[1:(length(    cycle_data$cycle)-1)] <-   tracer_background_inventory_change / (   cycle_data$date[2:length(  cycle_data$cycle)]-   cycle_data$date[1:(length(   cycle_data$cycle)-1)])
  
  
  # Compute the time rate of background tracer inventory change in the reference location
 # cycle_data$tracer_background_mean_concentration_reference_location [  cycle_data$season_label!='P']=NaN
  #cycle_data$tracer_background_mean_concentration_reference_location<- moving_smooth_fill(   cycle_data$tracer_background_mean_concentration_reference_location,
                                                                    #       smooth_number )
  #cycle_data$tracer_background_reference_location_inventory_change_rate=0 
  #tracer_background_reference_location_inventory_change= (  cycle_data$tracer_background_mean_concentration_reference_location[2:length(    cycle_data$cycle)]-  
   #                                        cycle_data$tracer_background_mean_concentration_reference_location[1:(length(    cycle_data$cycle)-1)]) *
    #(   cycle_data$integration_depth[2:length(    cycle_data$cycle)]+  cycle_data$integration_depth[1:(length(    cycle_data$cycle)-1)])/2
  
  #cycle_data$tracer_background_reference_location_inventory_change_rate[1:(length(    cycle_data$cycle)-1)] <-  tracer_background_reference_location_inventory_change/ (   cycle_data$date[2:length(  cycle_data$cycle)]-   cycle_data$date[1:(length(   cycle_data$cycle)-1)])
  
  
  # Moving smooth flux ------------------------------------------------------
  cycle_data$tracer_inventory_change_rate <- moving_smooth_fill(  cycle_data$tracer_inventory_change_rate,
                                                                 smooth_number )
  
  if (background_correction==3){
    cycle_data$tracer_background_inventory_change_rate<-  moving_smooth_fill_label (data= cycle_data$tracer_background_inventory_change_rate,  
                                                                                    
                                                                                    label= year_North_South( cycle_data$latitude,  cycle_data$date), 
                                                                                    smooth_number= smooth_number )$data
  } else{
    cycle_data$tracer_background_inventory_change_rate<-  moving_smooth_fill (data= cycle_data$tracer_background_inventory_change_rate,
                                                                                    smooth_number= smooth_number )
  }

  
 # cycle_data$tracer_background_reference_location_inventory_change_rate<- moving_smooth_fill(   cycle_data$tracer_background_reference_location_inventory_change_rate,
  #                                                                     smooth_number )
  
   cycle_data$entrainment<- moving_smooth_fill(   cycle_data$entrainment ,
                                                        smooth_number )
  
  
  cycle_data$vertical_diffusion<- moving_smooth_fill(   cycle_data$vertical_diffusion ,
                                                     smooth_number )
  
  cycle_data$vertical_advection<- moving_smooth_fill(   cycle_data$vertical_advection ,
                                                     smooth_number )
  
  
  cycle_data=data.frame(cycle=cycle_data$cycle,
                        longitude=cycle_data$longitude,
                        longitude_median_PS= cycle_data$longitude_median_PS,
                        latitude=cycle_data$latitude,
                        latitude_median_PS=cycle_data$latitude_median_PS,
                        date=cycle_data$date,
                        time= Convert_matlabtime( cycle_data$date),
                        integration_depth=cycle_data$   integration_depth,
                        mean_tracer_concentration=   cycle_data$tracer,
                        tracer_inventory_change_rate=cycle_data$tracer_inventory_change_rate,
                        tracer_background_inventory_change_rate= cycle_data$tracer_background_inventory_change_rate,
                        entrainment= cycle_data$entrainment,
                        vertical_advection= cycle_data$vertical_advection,
                        vertical_diffusion=cycle_data$vertical_diffusion  ,
                        season_label=    cycle_data$season_label)
                        
                        
                        
  return(  cycle_data)
}                                                                                                                                                        

