
Perform_tracer_budget <- function( 
    
  float_profile_data= float_profile_data_ancilary_parameter,
  Model_setting_list=Model_setting_list){
  
  # DESCRIPTION:
  # This function returns a data.frame containing model output from the tracer budget model 
  
  # PREREQUESTED FUNCTIONS (auxiliary function deposited in the file namely "auxi"）: 
  #   compute_five_terms_tracer_budget
  #   compute_DIC_gas_flux 
  #   moving_smooth_fill
  #   compute_oxygen_gas_flux        
  #   Gap_fill_variable     

  # INPUTS:
  #   Model_setting_list (list)       : a list file containing the model setting, which is returned  
  #                                     from the function entitled "tracer_budget_toolbox_settings" 
  #  float_profile_data (data.frame)  : a data.frame file containing the float data and ancillary parameters, which is returned  
  #                                     from the function entitled "Match_ancillary_data" 
  
  
  # OUTPUTS:
  #  data.frame file                  : a data.frame containing the processed float data 
  #  Creation DATE                    : JUNE 1, 2023  (Version 1.0)
  
  # Main procedures                   : 1. Create matrix for storing the Monte Carlo simulation results
  #                                     2. Loop to perform the Monte Carlo simulation
  #                                     3. Generate the error for each iteration 
  #                                     3. Compute the seawater tracer change, background correction and physical transport terms 
  #                                     4. Compute the EP term from salinity (optional step, depending on the model setting)
  #                                     5. compute the gas flux (optional step, depending on model setting )
  #                                     6. compute the biological term as a residual
  #                                     7. set the tracer budge terms to zero if there is missing values of tracer concentration
  #                                     8. add some basic information to the final output file (i.e., MLD, integration deph, and float information)
  #                                     9. return a data.frame file containing the various tracer budget terms  

# Create matrix for storing the Monte Carlo simulation ---------------------------------

  mean_tracer_concentration<- matrix(NaN,  
               nrow=n_distinct(float_profile_data$cycle),
               ncol=  Model_setting_list$iterations_uncertainty_simulation)
  
    tracer_dt<- matrix(NaN,  
                      nrow=n_distinct(float_profile_data$cycle),
                      ncol=  Model_setting_list$iterations_uncertainty_simulation)
    gas<- matrix(NaN,  
                 nrow=n_distinct(float_profile_data$cycle),
                 ncol=  Model_setting_list$iterations_uncertainty_simulation)
    EP<- matrix(NaN,  
                nrow=n_distinct(float_profile_data$cycle),
                ncol=  Model_setting_list$iterations_uncertainty_simulation)
    entrainment<- matrix(NaN,  
                         nrow=n_distinct(float_profile_data$cycle),
                         ncol=  Model_setting_list$iterations_uncertainty_simulation)
    
    vertical_diffusion<- matrix(NaN,  
                                nrow=n_distinct(float_profile_data$cycle),
                                ncol=  Model_setting_list$iterations_uncertainty_simulation)
    
    vertical_advection<- matrix(NaN,  
                                nrow=n_distinct(float_profile_data$cycle),
                                ncol=  Model_setting_list$iterations_uncertainty_simulation)
    
    Bio<- matrix(NaN,  
                 nrow=n_distinct(float_profile_data$cycle),
                 ncol=  Model_setting_list$iterations_uncertainty_simulation)
    

  
# Loop to perform the Monte Carlo simulation ---------------------------------

  float_profile_data_orignal <- float_profile_data  # Save the original float data before propagating the error 
  i=1
  

  for ( i in 1:     Model_setting_list$iterations_uncertainty_simulation){
    

  ## Generate the uncertainty for each iteration  ------------------------

   
    float_profile_data <-    float_profile_data_orignal
    float_profile_data$Kz_m2_s1<-     float_profile_data$Kz_m2_s1*(1+ rnorm(1,0,Model_setting_list$error_assignment[9,2]/100))

    float_profile_data$ekman_velocity_m_d1 <-  float_profile_data$ekman_velocity_m_d1*(1+rnorm(1,0,Model_setting_list$error_assignment[10,2])/100)
    
    float_profile_data$integration_depth_m <- floor(float_profile_data$integration_depth_m +  floor(rnorm(1,0,Model_setting_list$error_assignment[11,2])))
    float_profile_data$integration_depth_m[    float_profile_data$integration_depth_m<11]=11
  

   # Add the errors to the tracer concentration  

   
    if (  Model_setting_list$tracer==1){ # DIC
      tracer_error_line=4 # corresponding error line in the uncertainty form 
      CO2_gas_diffsuion_error <- rnorm(1,0,Model_setting_list$error_assignment[6,2] )/100
    }
    if (  Model_setting_list$tracer==2){ # NO3
      tracer_error_line=2
    }
    if (  Model_setting_list$tracer==3){ # oxygen 
      tracer_error_line=3
      O2_gas_diffsuion_error <- rnorm(1,0,Model_setting_list$error_assignment[7,2] )/100
      O2_gas_bubble_error <- rnorm(1,0,Model_setting_list$error_assignment[8,2] )/100
    }
    if (  Model_setting_list$tracer==4){ # TA
      tracer_error_line=4
    }
    if (  Model_setting_list$tracer==5){ # POC_bbp
      tracer_error_line=5
      
      float_profile_data$tracer_umol_kg<-  float_profile_data$tracer_umol_kg *(1+ 
        rnorm(1,0,Model_setting_list$error_assignment[     tracer_error_line,2] /100))
    }
    
    if (Model_setting_list$tracer!=5){
      float_profile_data$tracer_umol_kg<-  float_profile_data$tracer_umol_kg + 
        rnorm(1,0,Model_setting_list$error_assignment[     tracer_error_line,2] )
      
    }

  
  ## Compute five terms in the tracer budget--------
  # Five terms: 1.tracer change from float observation;  
  #             2. tracer change due to the background gradient
  #             3. vertical transport term (entrainment, advection and diffusion)
   
    tracer_budget <- compute_five_terms_tracer_budget( 
      cycle=float_profile_data$cycle,
      date=float_profile_data$date,
      longitude=float_profile_data$longitude_E,
      longitude_median_PS=float_profile_data$longitude_median_PS_E,
      latitude=float_profile_data$latitude_N,
      latitude_median_PS=float_profile_data$latitude_median_PS_N,
      temperature=float_profile_data$temperature_C,
      temperature_background=float_profile_data$temperature_background_C,
      temperature_background_reference_location=float_profile_data$temperature_background_reference_location_C,
      salinity =float_profile_data$salinity,
      salinity_background = float_profile_data$salinity_background,
      salinity_background_reference_location= float_profile_data$salinity_background_reference_location,
      tracer=float_profile_data$tracer_umol_kg,
      tracer_background=float_profile_data$tracer_background_umol_kg,
      tracer_background_reference_location=float_profile_data$tracer_background_reference_location_umol_kg,
      pressure=float_profile_data$pressure_m,
      Kz=float_profile_data$Kz_m2_s1,
      MLD=float_profile_data$MLD_m,
      ekman_v= float_profile_data$ekman_velocity_m_d1,
      season_label= float_profile_data$season_label,
      #  MLD_defination= Model_setting_list$MLD_defination,
       # integration_depth_error= integration_depth_error,
      smooth_number=  Model_setting_list$data_smooth_number ,
      integration_depth= float_profile_data$integration_depth_m,
      EP_term_computation=Model_setting_list$EP_term_computation,
      background_correction= Model_setting_list$background_correction) 
    
    
    if (Model_setting_list$integration_depth!=1){ # If the integration depth is not MLD, we choose to omit the entraiment term
      tracer_budget$entrainment=0
    }
    

  ## Perform the salinity budget to compute EP term -----------------------

    
    if (Model_setting_list$EP_term_computation==2 ){ # If choosing connection with salinity budget to solve the EP terms
      
        salinity_budget<-  compute_five_terms_tracer_budget( 
          
        cycle=float_profile_data$cycle,
        date=float_profile_data$date,
        longitude=float_profile_data$longitude_E,
        longitude_median_PS=float_profile_data$longitude_median_PS_E,
        latitude=float_profile_data$latitude_N,
        latitude_median_PS=float_profile_data$latitude_median_PS_N,
        temperature=float_profile_data$temperature_C,
        temperature_background=float_profile_data$temperature_background_C,
        temperature_background_reference_location=float_profile_data$temperature_background_reference_location_C,
        salinity =float_profile_data$salinity,
        salinity_background = float_profile_data$salinity_background,
        salinity_background_reference_location= float_profile_data$salinity_background_reference_location,
        tracer=float_profile_data$salinity,
        tracer_background=float_profile_data$salinity_background,
        tracer_background_reference_location=float_profile_data$salinity_background_reference_location,
        pressure=float_profile_data$pressure_m,
        Kz=float_profile_data$Kz_m2_s1,
        ekman_v= float_profile_data$ekman_velocity_m_d1,
        season_label= float_profile_data$season_label,
        #  MLD_defination= Model_setting_list$MLD_defination,
        # integration_depth_error= integration_depth_error,
        smooth_number=  Model_setting_list$data_smooth_number ,
        integration_depth= float_profile_data$integration_depth_m,
        EP_term_computation=Model_setting_list$EP_term_computation,
        background_correction= Model_setting_list$background_correction) 

        
        if (Model_setting_list$integration_depth!=1){
          salinity_budget$entrainment=0
        }
        
        salinity_budget$EP <- salinity_budget$tracer_inventory_change_rate- 
          salinity_budget$vertical_diffusion- salinity_budget$entrainment-
          salinity_budget$vertical_advection-salinity_budget$tracer_background_inventory_change_rate

        
        # Moving smooth salinity EP term
        salinity_budget$EP <- moving_smooth_fill(   salinity_budget$EP,
                                                    smooth_number=Model_setting_list$data_smooth_number  ) 
        
        tracer_budget$EP <-     salinity_budget$EP * (    float_profile_data$tracer_umol_kg[1]/float_profile_data$salinity[1])
        
      } else{
        tracer_budget$EP <- 0
      } # bracket for "if (Model_setting_list$EP_term_computation==2 )"
      

    


    ## Compute the gas exchange term -------------------------------------------

    
    tracer_budget$gas=0
    if (Model_setting_list$tracer==1){ # DIC
      
      
      tracer_budget$gas <- compute_DIC_gas_flux  ( cycle=float_profile_data$cycle,
                                                   temperature=float_profile_data$temperature_C,
                                                   salinity=float_profile_data$salinity,
                                                   pressure=float_profile_data$pressure_m,
                                                   pCO2_air=float_profile_data$pCO2_air_uatm,
                                                   pCO2_water=   float_profile_data$pCO2_water_uatm,
                                                   wind_speed=float_profile_data$wind_speed_m_s,
                                                   ice_coverage= float_profile_data$ice_coverage_percent,
                                                   ks_error=    CO2_gas_diffsuion_error,
                                                   gas_model=Model_setting_list$gas_model)
      
    } # bracket for "if (tracer =="DIC")"
    
    if (Model_setting_list$tracer==3){ # oxygen
      

      tracer_budget$gas <- compute_oxygen_gas_flux ( cycle=float_profile_data$cycle,
                                                                 temperature=float_profile_data$temperature_C,
                                                                 salinity=float_profile_data$salinity,
                                                                 pressure=float_profile_data$pressure_m,
                                                                 oxygen = float_profile_data$tracer_umol_kg1, 
                                                                 sea_level_pressure = float_profile_data$sea_level_pressure_Pa,
                                                                 wind_speed=float_profile_data$wind_speed_m_s,
                                                                 ice_coverage= float_profile_data$ice_coverage_percent,
                                                                 ks_oxygen_error=      O2_gas_diffsuion_error, 
                                                                 kb_oxygen_error=      O2_gas_bubble_error, 
                                                                 kc_oxygen_error=      O2_gas_bubble_error ,
                                                                 gas_model=Model_setting_list$gas_model)
      
    } # bracket for "if (tracer =="3")
    
    tracer_budget$gas <- moving_smooth_fill(    tracer_budget$gas,
                                                smooth_number=Model_setting_list$data_smooth_number  )
    
    
 

 ## Compute the residual term as biology ---------------------------------

    
    
    if (Model_setting_list$integration_depth!=1){
      tracer_budget$entrainment=0
    }
    
    
    tracer_budget$Bio <- tracer_budget$tracer_inventory_change_rate -
      tracer_budget$entrainment- tracer_budget$gas- 
      tracer_budget$vertical_diffusion-
      tracer_budget$EP- tracer_budget$vertical_advection- 
      tracer_budget$tracer_background_inventory_change_rate
    
    # Set the other terms to zero when tracer concentration is not available
    tracer_budget$EP[   is.na(tracer_budget$Bio)]=NaN
    tracer_budget$gas[   is.na(tracer_budget$Bio)]=NaN
    tracer_budget$tracer_background_inventory_change_rate[   is.na(tracer_budget$Bio)]=NaN
    tracer_budget$vertical_advection[   is.na(tracer_budget$Bio)]=NaN
    tracer_budget$vertical_diffusion[   is.na(tracer_budget$Bio)]=NaN
    tracer_budget$entrainment[   is.na(tracer_budget$Bio)]=NaN
    tracer_budget$tracer_inventory_change_rate[   is.na(tracer_budget$Bio)]=NaN
    
    # Remove the data during the unproductive season if we choose the third background correction approach
  #  if (Model_setting_list$background_correction==3){
      
   #   tracer_budget$tracer_background_dt_mmol_m2_d1[  tracer_budget$season_label=="UP"]=NaN
    #  tracer_budget$tracer_inventory_change_rate[  tracer_budget$season_label=="UP"]=NaN
     # tracer_budget$Bio[  tracer_budget$season_label=="UP"]=NaN
      #tracer_budget$EP[  tracer_budget$season_label=="UP"]=NaN
      #tracer_budget$gas[  tracer_budget$season_label=="UP"]=NaN
      #tracer_budget$tracer_background_inventory_change_rate[   tracer_budget$season_label=="UP"]=NaN
      #tracer_budget$vertical_advection[   tracer_budget$season_label=="UP"]=NaN
      #tracer_budget$vertical_diffusion[   tracer_budget$season_label=="UP"]=NaN
      #tracer_budget$entrainment[   tracer_budget$season_label=="UP"]=NaN
      
    #}
    

## Save the result for each iteration --------------------------------------

   
    mean_tracer_concentration[,i] <-    tracer_budget$mean_tracer_concentration
    tracer_dt[,i] <-    tracer_budget$tracer_inventory_change_rate
    gas[,i] <-  tracer_budget$gas
    EP[,i] <-  tracer_budget$EP
    entrainment[,i] <-  tracer_budget$entrainment
    vertical_diffusion[,i] <-  tracer_budget$vertical_diffusion
    vertical_advection[,i] <-  tracer_budget$vertical_advection
    Bio[,i] <-  tracer_budget$Bio
    
    
    # Print the progress 
    print(paste("Uncertainty simulation:", i,"/",  Model_setting_list$iterations_uncertainty_simulation,sep=""))
    
    
  } # Bracket for " for ( i in 1:     Model_setting_list$iterations_uncertainty_simulation)"
  
  
  

# Compile the final output  -------------------------------------------


  tracer_budget=data.frame( WMIOD=   Model_setting_list$WMOID,
                            tracer=  Model_setting_list$tracer,
                            cycle=tracer_budget$cycle,
                            longitude_E= tracer_budget$longitude,
                            longitude_median_PS_E= tracer_budget$longitude_median_PS,
                            latitude_N= tracer_budget$latitude,
                            latitude_median_PS_E= tracer_budget$ latitude_median_PS,
                            season_label=tracer_budget$season_label,

                            date=tracer_budget$date,
                            time=Convert_matlabtime( tracer_budget$date),
                            
                            mean_tracer_concentration_mmol_m3= rowMeans( mean_tracer_concentration,na.rm=T),
                            mean_tracer_concentration_error_mmol_m3= matrixStats::rowSds( mean_tracer_concentration,na.rm=T),
                            
                            tracer_dt_mmol_m2_d1= rowMeans(    tracer_dt,na.rm=T),
                            tracer_dt_error_mmol_m2_d1=matrixStats::rowSds(  tracer_dt,na.rm=T),
                            
                            gas_mmol_m2_d1= rowMeans(    gas,na.rm=T),
                            gas_error_mmol_m2_d1=matrixStats::rowSds(  gas,na.rm=T),
                            
                            EP_mmol_m2_d1= rowMeans(  EP,na.rm=T),
                            EP_error_mmol_m2_d1=matrixStats::rowSds(  EP,na.rm=T),
                            
                            entrainment_mmol_m2_d1= rowMeans(  entrainment,na.rm=T),
                            entrainment_error_mmol_m2_d1=matrixStats::rowSds( entrainment,na.rm=T),
                            
                            vertical_diffusion_mmol_m2_d1= rowMeans(       vertical_diffusion,na.rm=T),
                            vertical_diffusion_error_mmol_m2_d1=matrixStats::rowSds(      vertical_diffusion,na.rm=T),

                            vertical_advection_mmol_m2_d1= rowMeans(            vertical_advection,na.rm=T),
                            vertical_advection_error_mmol_m2_d1=matrixStats::rowSds(          vertical_advection,na.rm=T),
                            
                            tracer_background_dt_mmol_m2_d1 = tracer_budget$tracer_background_inventory_change_rate,
                            Bio_mmol_m2_d1 =rowMeans(       Bio,na.rm=T),
                            Bio_error_mmol_m2_d1=matrixStats::rowSds(      Bio,na.rm=T))
  
  # Add the information about the reference location if users select the background correction method 3
  if (Model_setting_list$background_correction==3){
    tracer_budget$latitude_median_PS_N= tracer_budget$latitude_median_PS
    tracer_budget$season_label=tracer_budget$season_label
  } else{
    tracer_budget$latitude_median_PS_N= "Not applicable"
    tracer_budget$season_label= "Not applicable"
  }

  # Add the mixed layer depth and integration depth to the output file
  
  depth_information = data.frame (cycle= float_profile_data_orignal$cycle,
                                  integration_depth_m =float_profile_data_orignal$integration_depth,
                                 MLD_m =float_profile_data_orignal$MLD)
  
  depth_information =distinct(  depth_information) 
  tracer_budget=left_join(  tracer_budget,  depth_information, "cycle")
  
  
  tracer_budget$tracer[  tracer_budget$tracer==1]="DIC"
  tracer_budget$tracer[  tracer_budget$tracer==2]="NO3"
  tracer_budget$tracer[  tracer_budget$tracer==3]="Oxygen"
  tracer_budget$tracer[  tracer_budget$tracer==4]="TA"
  tracer_budget$tracer[  tracer_budget$tracer==5]="POC_bbp"
  
  if (Model_setting_list$tracer==3){
    tracer_budget$oxygen_sensor_calibration=float_profile_data$oxygen_sensor_calibration[1]
  } # add the sensor calibration information if selected tracer is oxygen 
 
  
  return(   tracer_budget)
}                                                                                                                                                        


