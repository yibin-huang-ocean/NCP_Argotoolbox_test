download_import_process_float_data <- function (
    Model_setting_list
){
  
  # DESCRIPTION:
  # This function returns a data.frame containing the processed float data 

  # PREREQUESTED FUNCTIONS (auxiliary function deposited in the file namely "auxi"）: 
  #   load_float_data  
  #   compute_DIC_pCO2_Canyon_B      
  #   compute_TA_Canyon_B            
  #   bbp_qc_standard              
  #   Gap_fill_variable     
  #   Interp_profile
  
  # INPUTS:
  #   Model_setting_list (list): a list file containing the model setting, which is returned
  #                              from the function entitled "tracer_budget_toolbox_settings"
  
  # OUTPUTS:
  #  data.frame file: a data.frame containing the processed float data
  #  Creation DATE: JUNE 1, 2023 (Version 1.0)
  
  # Main procedures:
  # 1. Download and import the float data from GDAC
  # 2. Compute carbonate system parameters if the user selects DIC or TA
  # 3. Quality-control the raw backscattering data if the user selects POC_bbp
  # 4. Vertical interpolation of float data into 1-meter
  # 5. Remove the float data based on the user-defined cycle list
  # 6. Gap-fill the time-series missing tracer value at each depth
  # 7. Extract the oxygen sensor calibration information
  # 8. Return the processed float data (in the format of data.frame) 
  
  print("Start downloading the float data")

# Download and import the float data using ONE-Argo toolbox --------

  path_code_ONE_Argo_toolbox= paste(    Model_setting_list$path_NCP_toolbox,
                                       "Ancillary data and toolbox/Toolbox/ONE-ARGO toolbox",
                                       sep="")
  setwd(  path_code_ONE_Argo_toolbox)
  
  float_profile_data= load_float_data_revised( float_ids=    Model_setting_list$WMOID, # specify WMO number
                               # float_profs=OSP_data$float_profs, # specify selected profiles
                               #   variables= variables, # load all the variables
                                format="dataframe" # specify format;  
  ) # Note: "load_float_data" archived from the ONE-ARGO-R toolbox is slightly modified in 
    # in order to extract the oxygen sensor calibration information 
  
  
  float_profile_data=subset(  float_profile_data,  
                              float_profile_data$CYCLE_NUMBER>0 &
                                float_profile_data$TEMP_ADJUSTED > -999 &
                                float_profile_data$PSAL_ADJUSTED> -999&
                                float_profile_data$LONGITUDE> -999 &
                                float_profile_data$PRES_ADJUSTED>-999)
 
   # Check if there are adjusted physical CTD data available in the float
  if (length(float_profile_data$CYCLE_NUMBER)<1){
    print(   "Error: the code terminates because of no quality-controlled physical data found in this float")
    return()
  }
  
  
  # convert the date and WMOID
  float_profile_data$date=   float_profile_data$JULD+ as.numeric ( floor((difftime("1950-01-01","0000-01-01", units="day")))+1) 
  float_profile_data$WMOID=as.numeric( substring(   float_profile_data$WMOID, 2))
  
  # Extract the oxygen calibration method 
 oxygen_sensor_calibration <-   float_profile_data$oxygen_sensor_calibration[1]
  
 #Check if the user-selected tracer data  is available in this float ----------------

  
  sensor_exist= 'FALSE'
  if ( Model_setting_list$tracer==1){
    sensor_exist<- "PH_IN_SITU_TOTAL_ADJUSTED"%in%colnames(   float_profile_data) &
      "DOXY_ADJUSTED"%in%colnames(   float_profile_data) 
      
  }
  if ( Model_setting_list$tracer==2){ # NO3 as a tracer
    sensor_exist<- "NITRATE_ADJUSTED"%in%colnames(   float_profile_data) 
  }
  

  
  if ( Model_setting_list$tracer==4 | Model_setting_list$tracer==3 ){
    sensor_exist<- "DOXY_ADJUSTED"%in%colnames(   float_profile_data)
  }
  
  
  if ( Model_setting_list$tracer==5){
    sensor_exist<- "BBP700"%in%colnames(   float_profile_data)
  }
  
  
  if (sensor_exist=="FALSE"){
    print(   "Error: the code terminates because of no corresponding sensor or no quality-controlled tracer data found in this float")
    return()
  }
 
  
  if (   Model_setting_list$tracer==1){ # If 1. DIC
    
    # Compute the carbonate parameters ---------------------------------------
    
    # remove the bad quality data 
    float_profile_data$PH_IN_SITU_TOTAL_ADJUSTED [which(  float_profile_data$PH_IN_SITU_TOTAL_ADJUSTED_QC!=1 &  float_profile_data$PH_IN_SITU_TOTAL_ADJUSTED_QC!=2)]=NaN
    float_profile_data$DOXY_ADJUSTED[which(  float_profile_data$DOXY_ADJUSTED_QC!=1 &  float_profile_data$DOXY_ADJUSTED_QC!=2)]=NaN
    
    # Terminate the code if no data pass the quality control
    if ( all(is.na(    float_profile_data$PH_IN_SITU_TOTAL_ADJUSTED)) |
         all(is.na(    float_profile_data$DOXY_ADJUSTED)) ){
      print(   "Error: the code terminates because of tracer data with no good quality found in this float")
      return()
    }
    
    DIC=rep(NaN, length(float_profile_data$CYCLE_NUMBER))
    pCO2 =rep(NaN, length(float_profile_data$CYCLE_NUMBER))
    print("Start computating DIC and seawater pCO2")
    
    pH_oxygen_no_NA <- which(!is.na(float_profile_data$DOXY_ADJUSTED) & !is.na(float_profile_data$PH_IN_SITU_TOTAL_ADJUSTED)   )
    
   Carbonate_parameters <- compute_DIC_pCO2_Canyon_B(date=float_profile_data$date[    pH_oxygen_no_NA], 
                                                     latitude=float_profile_data$LATITUDE[    pH_oxygen_no_NA], 
                                                     longitude=float_profile_data$LONGITUDE[    pH_oxygen_no_NA], 
                                                     pressure=float_profile_data$PRES_ADJUSTED[    pH_oxygen_no_NA], 
                                                     temperature=float_profile_data$TEMP_ADJUSTED[    pH_oxygen_no_NA], 
                                                     salinity=float_profile_data$PSAL_ADJUSTED[    pH_oxygen_no_NA],
                                                     oxygen=float_profile_data$DOXY_ADJUSTED[    pH_oxygen_no_NA] ,
                                                     pH=float_profile_data$PH_IN_SITU_TOTAL_ADJUSTED[    pH_oxygen_no_NA],
                                                     Car_Equ_Cons_K1_K2 = Model_setting_list$Car_Equ_Cons_K1_K2,
                                                     Car_Equ_Cons_Ks= Model_setting_list$Car_Equ_Cons_Ks,
                                                     Total_boron_concentration= Model_setting_list$Total_boron_concentration
                                                     
                                )
    
  
   DIC[    pH_oxygen_no_NA]<- Carbonate_parameters$DIC*1000000
   pCO2[    pH_oxygen_no_NA]<- Carbonate_parameters$pCO2insitu
    print("Success in DIC and pCO2 estimate")
    

    
    float_profile_data= data.frame(
      pressure_m=   float_profile_data$PRES_ADJUSTED,
      WMOID=   float_profile_data$WMOID,
      cycle=   float_profile_data$CYCLE_NUMBER,
      date=  float_profile_data$date,
      longitude_E=  float_profile_data$LONGITUDE,
      latitude_N=  float_profile_data$LATITUDE,
      temperature_C=  float_profile_data$TEMP_ADJUSTED,
      salinity=  float_profile_data$PSAL_ADJUSTED,
      tracer_umol_kg1= DIC,
      pCO2_water_uatm=pCO2)
 
  }  # bracket for (tracer=="DIC")
  
  
  if (Model_setting_list$tracer==4 ){ # oxygen
  
    float_profile_data$DOXY_ADJUSTED[which(  float_profile_data$DOXY_ADJUSTED_QC!=1 &  float_profile_data$DOXY_ADJUSTED_QC!=2)]=NaN
    
    # Terminate the code if no data pass the quality control
    if ( all(is.na(    float_profile_data$DOXY_ADJUSTED)) ){
      print(   "Error: the code terminates because of tracer data with no good quality  found in this float")
      return()
    }
    
    
    print("Start computating TA")
    # Canyon_B algorithm 
    
    TA=rep(NaN, length(float_profile_data$CYCLE_NUMBER))
    
    oxygen_no_NA <- which(!is.na(float_profile_data$DOXY_ADJUSTED)   )
    
    TA[       oxygen_no_NA] <- compute_TA_Canyon_B(date=float_profile_data$date[        oxygen_no_NA], 
                                                     latitude=float_profile_data$LATITUDE[       oxygen_no_NA], 
                                                     longitude=float_profile_data$LONGITUDE[       oxygen_no_NA], 
                                                     pressure=float_profile_data$PRES_ADJUSTED[       oxygen_no_NA], 
                                                     temperature=float_profile_data$TEMP_ADJUSTED[        oxygen_no_NA], 
                                                     salinity=float_profile_data$PSAL_ADJUSTED[       oxygen_no_NA],
                                                     oxygen=float_profile_data$DOXY_ADJUSTED[     oxygen_no_NA] 
    )
    
    
    print("Success in TA estimate")
    
    float_profile_data= data.frame(
      pressure_m=   float_profile_data$PRES_ADJUSTED,
      WMOID=   float_profile_data$WMOID,
      cycle=   float_profile_data$CYCLE_NUMBER,
      date=  float_profile_data$date,
      longitude_E=  float_profile_data$LONGITUDE,
      latitude_N=  float_profile_data$LATITUDE,
      temperature_C=  float_profile_data$TEMP_ADJUSTED,
      salinity=  float_profile_data$PSAL_ADJUSTED,
      tracer_umol_kg1= TA
      
    )
    
  }
  
  
  if (   Model_setting_list$tracer==3){ # If 3. oxygen
   
    float_profile_data$DOXY_ADJUSTED[which(  float_profile_data$DOXY_ADJUSTED_QC!=1 &  float_profile_data$DOXY_ADJUSTED_QC!=2)]=NaN
    
    float_profile_data= data.frame(
      pressure_m=   float_profile_data$PRES_ADJUSTED,
      WMOID=   float_profile_data$WMOID,
      cycle=   float_profile_data$CYCLE_NUMBER,
      date=  float_profile_data$date,
      longitude_E=  float_profile_data$LONGITUDE,
      latitude_N=  float_profile_data$LATITUDE,
      temperature_C=  float_profile_data$TEMP_ADJUSTED,
      salinity=  float_profile_data$PSAL_ADJUSTED,
      tracer_umol_kg1= float_profile_data$DOXY_ADJUSTED)
    
    
  }  # bracket for (tracer=="oxygen")
  
  
  if (   Model_setting_list$tracer==2){ # If NO3
    
    float_profile_data= data.frame(
      pressure_m=   float_profile_data$PRES_ADJUSTED,
      WMOID=   float_profile_data$WMOID,
      cycle=   float_profile_data$CYCLE_NUMBER,
      date=  float_profile_data$date,
      longitude_E=  float_profile_data$LONGITUDE,
      latitude_N=  float_profile_data$LATITUDE,
      temperature_C=  float_profile_data$TEMP_ADJUSTED,
      salinity=  float_profile_data$PSAL_ADJUSTED,
      tracer_umol_kg1= float_profile_data$NITRATE_ADJUSTED)
    
    
  }  # bracket for (tracer=="oxygen")
  
  
  if (   Model_setting_list$tracer==5 ){ # If POC
    
    print("Start bbp quality control")
    
    # Quality-control the bbp -------------------------------------------------
    
    float_profile_data$bbp_qc=bbp_qc_standard ( cycle=float_profile_data$CYCLE_NUMBER,
                              pressure=float_profile_data$PRES,
                              bbp_raw= float_profile_data$BBP700)
    
    # Convert bbp into the POC concentration
    
    if (Model_setting_list$bbp_to_POC_conversion==1){# Briggs et al., (2011)
      float_profile_data$POC=  3490.2*float_profile_data$bbp_qc  # umol C kg-1
      
    }
    
    if (Model_setting_list$bbp_to_POC_conversion==2){# Table 8 in Johnson et al., (2017)
      float_profile_data$POC=  (9.776 *10^4*float_profile_data$bbp_qc^1.166)/12  # umol C kg-1
      
    }
  
    float_profile_data= data.frame(
      pressure_m=   float_profile_data$PRES_ADJUSTED,
      WMOID=   float_profile_data$WMOID,
      cycle=   float_profile_data$CYCLE_NUMBER,
      date=  float_profile_data$date,
      longitude_E=  float_profile_data$LONGITUDE,
      latitude_N=  float_profile_data$LATITUDE,
      temperature_C=  float_profile_data$TEMP_ADJUSTED,
      salinity=  float_profile_data$PSAL_ADJUSTED,
      tracer_umol_kg1=  float_profile_data$POC)

    print("Start bbp data quality control")
    
  }  # Bracket for  "if (   Model_setting_list$tracer==5)"
  


  
   ## double-check if the adjusted tracer data available -----------------------------
  if (all(is.na(float_profile_data$tracer_umol_kg1))| 
      all(is.na(float_profile_data$temperature_C))| 
      all(is.na(float_profile_data$salinity))){
    print(   "Error: the code terminates because of no adjusted data with good qualityf found in this float")
    return()
  }
  
# Extrapolate the data into 1-meter ---------------------------------------

  # note: this function requires the file with 
  # the first column being the pressure
  float_profile_data <- Interp_profile(
    float_data =  float_profile_data,
    column_depth=1, 
    column_profile=3, 
    column_variable_start=1,  
    column_variable_end= ncol ( float_profile_data), 
    min_depth= 1  , # star depth   
    interval=1 # depth interval  
  )

 # if (oxygen_sensor_calibration ==2){
  #  print("Note: the float oxygen sensor was calibrated against WOA oxygen climatology (typically achiving the accuracy of ~3%) so oxygen-based NCP estimate may carry large error.")
   # float_profile_data$oxygen_sensor_calibration="WOA calibration"
  #}
  


# Double check if the surface data is completed ---------------------------
  float_profile_data_surface=subset( float_profile_data, 
                                     float_profile_data$pressure_m==1)
  
  if (  n_distinct(float_profile_data_surface)<5){
    print(   "Error: the code terminates because the cycles with complete surface data are less than 5 ")
    return()
  }

  # remove the data based on the cycle numbers provided by the user  -----------------------------------------------------
  float_profile_data= filter(  float_profile_data,  !float_profile_data$cycle %in%  Model_setting_list$cycle_filter)
  
  float_profile_data$oxygen_sensor_calibration= oxygen_sensor_calibration
  if (oxygen_sensor_calibration ==1){
    print("Note: the float oxygen sensor was calibrated against the air measurment (typically achiving the accuracy of ~1%) so oxygen-based NCP estimate may be reliable.")
    float_profile_data$oxygen_sensor_calibration="Air calibration"
  }
  
  
# Gap-fill the missing value of tracer over time -----------------------------------------------------

  
  
  if  (Model_setting_list$tracer_gap_filling==1){
   
    float_profile_data$tracer_umol_kg1= Gap_fill_variable (depth=float_profile_data$pressure_m,
                                                  date=float_profile_data$date,
                                                  variable=float_profile_data$tracer_umol_kg1)
    
    
    if (  Model_setting_list$tracer==1){
    float_profile_data$pCO2_water_uatm= Gap_fill_variable (depth=float_profile_data$pressure_m,
                                                        date=float_profile_data$date,
                                                        variable=float_profile_data$pCO2_water_uatm)
    } # Bracket for  Model_setting_list$tracer==1
  } # Bracket for if  (Model_setting_list$tracer_gap_filling==1)
  

  print("Success in float data vertical interpolation")
  
  
  

# Only Retain a single cycle for each sampling date -----------------
# because the tracer budget model only solve the daily flux
#  float_profile_data$date_pressure_label= paste(float_profile_data$pressure_m,
 #                                              floor(float_profile_data$date))
  #float_profile_data <-   float_profile_data [!duplicated (  float_profile_data$date_pressure_label), ]   

  
  return(float_profile_data)
  
} # bracket for  "float_data_download_import_process"
