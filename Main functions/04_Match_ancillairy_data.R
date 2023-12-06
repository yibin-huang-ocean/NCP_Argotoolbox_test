# 11/29/2023: MC/HF Change the NCEP matchup with unique function match_NCEP 
Match_ancillary_data <- function( float_profile_data=float_profile_data,
                                  Model_setting_list =  Model_setting_list){
  # DESCRIPTION:
  # This function returns a data.frame containing the processed float data with paired ancillary parameters.
  
  # PREREQUISITE FUNCTIONS (auxiliary functions deposited in the file named "auxil"):
  # compute_median_location_productive_season
  # match_WOA_annual
  # match_DIC_NNGv2LDEO
  # match_TA_NNGv2LDEO
  # Convert_matlabtime
  # match_NCEP
  # compute_MLD
  # match_monthly_climatology_ocean_color
  # match_annual_ocean_color
  # convert_XCO2_to_pCO2
  # match_ekman_pumping_velocity
  
  # INPUTS:
  # float_profile_data (data.frame): A data.frame file containing the processed float data, which is returned
  #                                  from the function entitled "float_data_download_import_process".
  # Model_setting_list (list): A list file containing the model settings, which is returned
  #                            from the function entitled "tracer_budget_toolbox_settings".

  # OUTPUTS:
  # data.frame file: A data.frame containing the processed float data with paired ancillary parameters.
  # Creation DATE: June 1, 2023 (Version 1.0)
  
  # Main procedures:
  # 1. Compute the float median location during each productive season.
  # 2. Match various ancillary parameters based on the tracer and model settings.
  # 3. Compute the mixed layer depth.
  # 4. Smooth the integration depth (over time).
  
  # Step 1: Create a data frame to store the ancillary data  ----------
  

  ancilary_data_per_cycle <- subset(float_profile_data,
                                    float_profile_data$pressure_m==1)
  
  ancilary_data_per_cycle=data.frame(  cycle=ancilary_data_per_cycle$cycle,
                                       longitude=ancilary_data_per_cycle$longitude_E,
                                       latitude=ancilary_data_per_cycle$latitude_N,
                                       date=ancilary_data_per_cycle$date,
                                       temperature=ancilary_data_per_cycle$temperature_C,
                                       salinity=ancilary_data_per_cycle$salinity)
  ancilary_data_per_cycle$time= Convert_matlabtime( ancilary_data_per_cycle$date)
  
  
  # Step 2: Collocate with wind speed, sea-level pressure, and ice coverage  --------
  
  if (  Model_setting_list$tracer==1 | Model_setting_list$tracer== 3){ # if tracer is DIC or oxygen
    
    
    print("Start matching up with U-wind speed")
    
    U_wind<- match_NCEP ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                       "Ancillary data and toolbox/Ancilary data/NCEP_II/uwind", sep="") ,
                          
                          date =   ancilary_data_per_cycle$date,
                          longitude=   ancilary_data_per_cycle$longitude,
                          latitude=   ancilary_data_per_cycle$latitude, 
                          variable='uwnd') # 11/29/2023: MC remlace the match_NCEP_NCP by match_NCEP
    
    print("Success in U_wind matchup")
    
    print("Start matching up with V-wind speed")
    V_wind <- match_NCEP ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                        "Ancillary data and toolbox/Ancilary data/NCEP_II/vwind", sep="") ,
                           
                           date =   ancilary_data_per_cycle$date,
                           longitude=   ancilary_data_per_cycle$longitude,
                           latitude=   ancilary_data_per_cycle$latitude, 
                           variable='vwnd') # 11/29/2023: MC replace the match_NCEP_NCP by match_NCEP
    
    print("Success in V_wind matchup")
    
    ancilary_data_per_cycle$wind_speed_m_s <-   as.numeric(sqrt( U_wind^2+   V_wind ^2))
    
    # Obtain the sea-level pressure 
    ancilary_data_per_cycle$sea_level_pressure_Pa <- match_NCEP( path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                              "Ancillary data and toolbox/Ancilary data/NCEP_II/sea-level pressure", sep="") ,
                                                                                 
                                                                                 date =   ancilary_data_per_cycle$date,
                                                                                 longitude=   ancilary_data_per_cycle$longitude,
                                                                                 latitude=   ancilary_data_per_cycle$latitude, 
                                                                                 variable='mslp') # 11/29/2023: MC replace the match_NCEP_NCP by match_NCEP
    
    
    
    print("Success in sea-level pressure matchup")
    
    
    # # Obtain the sea ice coverage -------------------------------------------
    
    
   ancilary_data_per_cycle$ice_coverage_percent <-  match_NCEP ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                "Ancillary data and toolbox/Ancilary data/NCEP_II/Ice coverage", sep="") ,
                                                                                   
                                                                                   date =   ancilary_data_per_cycle$date,
                                                                                   longitude=   ancilary_data_per_cycle$longitude,
                                                                                   latitude=   ancilary_data_per_cycle$latitude, 
                                                                                   variable='icec') # 11/29/2023: MC replace the match_NCEP_NCP by match_NCEP
      
    
    print("Success in ice coverage  matchup")
  } # Bracket for "if (  Model_setting_list$tracer==1 | Model_setting_list$tracer== 3)"
  
  
  
   # Collocate with air pCO2 (computed from relative humidity and air --------
  
  
  if ( Model_setting_list$tracer==1){
    
    # Obtain the relative humidity 
    ancilary_data_per_cycle$relative_humidity_percent <- match_NCEP( path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                  "Ancillary data and toolbox/Ancilary data/NCEP_II/relative humidity_NCEP_I", sep="") ,
                                                                                     
                                                                                     date =   ancilary_data_per_cycle$date,
                                                                                     longitude=   ancilary_data_per_cycle$longitude,
                                                                                     latitude=   ancilary_data_per_cycle$latitude, 
                                                                                     variable='rhum')# 11/29/2023: MC replace the match_NCEP_NCP by match_NCEP
    
    
    
    # print("Success in relative humidity matchup")
    
    # Obtain the air xCO2 
    
    ancilary_data_per_cycle$xCO2_air_uatm <- collocate_air_xCO2(
      path_code=  paste( Model_setting_list$path_NCP_toolbox,  
                               "Ancillary data and toolbox/Ancilary data/air_xCO2", sep="") ,
      float_date= ancilary_data_per_cycle$date,
      float_latitude=     ancilary_data_per_cycle$latitude,
      float_longitude=     ancilary_data_per_cycle$longitude)
    
    ancilary_data_per_cycle$pCO2_air_uatm<- convert_XCO2_to_pCO2(temperature=ancilary_data_per_cycle$temperature, 
                                                                 salinity=ancilary_data_per_cycle$salinity, 
                                                                 xCO2_air=ancilary_data_per_cycle$xCO2_air_uatm,
                                                                 sea_level_pressure=ancilary_data_per_cycle$sea_level_pressure,
                                                                 relative_humidity=ancilary_data_per_cycle$relative_humidity_percent
    )
    
    print("Success in air pCO2 matchup")
    
  } #loop for "if    Model_setting_list$tracer=="DIC""
  
  
  
  
  # Step 3: Collocate with the tracer background gradient 
  
  ancilary_data_per_cycle$Kz_m2_s1=  Model_setting_list$diapycnal_diffusivity
  
  
  
  if (Model_setting_list$integration_depth>9){ # Fixed depth
    
    ancilary_data_per_cycle$integration_depth_m=Model_setting_list$integration_depth
    
    
  }
  
  # Match the euphotic zone depth -------------------------------------------
  if (Model_setting_list$integration_depth==2|
      Model_setting_list$integration_depth==3){
    
    print("Start matching up with monthly euphotic zone depth")
    
    # The euphotic zone is computed from the remotely sensed Chla based on the 
    # empirical algorithm.
    # Equation: Zeu= 34*(Chla) ^(−0.39); Reference: Eq. 10 in Lee (2007) 
    # Chl-a data source: https://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_034c_01ca_73a8.html
    # MODIS Aqua chla mapped mon 9km
    
    surface_chl <-match_monthly_ocean_color  ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                       "Ancillary data and toolbox/Ancilary data/Chla", sep="") , 
                                                          date=     ancilary_data_per_cycle$date ,
                                                          longitude=  ancilary_data_per_cycle$longitude,
                                                          latitude=  ancilary_data_per_cycle$latitude  ) 
    
    # compute the euphotic zone from surface chl-a based on the Eq. 10 in Lee et al., 2007
    ancilary_data_per_cycle$zeu_m=  floor( 34*(  surface_chl) ^(-0.39))
    
    # fill the missing data with the monthly or annual climatolgy
    na_line= is.na(ancilary_data_per_cycle$zeu_m)
    if (   length(   na_line)>0){
      ancilary_data_per_cycle$zeu_m[  na_line] <- match_euphotic_zone_monthly_climatology (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                        "Ancillary data and toolbox/Ancilary data/Euphotic zone_climatology", sep="") ,
                                                                                           longitude=ancilary_data_per_cycle$longitude[   na_line],
                                                                                           latitude=ancilary_data_per_cycle$latitude[   na_line],
                                                                                           date=ancilary_data_per_cycle$date[   na_line])
      
    }
   
    
    if (Count_No_NA(   ancilary_data_per_cycle$zeu_m)< 5){
      
      print(   "Error: the code terminates because of few matchup of remotely sensed euphotic zone in this float")
      return()
      
    }
   
    ancilary_data_per_cycle$zeu_m=floor(approx(        ancilary_data_per_cycle$date,    
                                                       ancilary_data_per_cycle$zeu_m,
                                                       ancilary_data_per_cycle$date,
                                                       rule=2)$y)
    if (Model_setting_list$integration_depth==3){
      ancilary_data_per_cycle$zeu_m=mean(     ancilary_data_per_cycle$zeu_m,
                                              na.rm=T)
    }
    ancilary_data_per_cycle$integration_depth_m=ancilary_data_per_cycle$zeu_m
    print("Success in euphotic zone matchup")

    
  } # Bracket for " if (Model_setting_list$integration_depth==2|"
  
  
  # Collocate the ekman pumping velocity ------------------------------------
  # ekman pumping velocity source: https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAstress1day_LonPM180.html
  # ASCAT 0.25°； daily (m s-1)
  
 if (Model_setting_list$ekman_pumping_velocity==1){ # Match to real time ekman pumping velocity
  
   
   # Trouble shooting if ERDDAP is not responding 
   tryCatch(  ancilary_data_per_cycle$ekman_velocity_m_d1 <- match_parameter_ERDAPP_surface(
     date=     ancilary_data_per_cycle$date ,
     longitude=  ancilary_data_per_cycle$longitude,
     latitude=  ancilary_data_per_cycle$latitude,
     dataset="erdQMstress1day",
     url="https://coastwatch.pfeg.noaa.gov/erddap/",
     parameter_name="upwelling",
     zcoord_included="yes",
     zcoord_level=0,
     zcoord_name="altitude")*3600*24 
     , 
             error = function(e) print("ERDDAP is not responding so switching to match with local, climatology of ekman pumping velocity"))  
   
   # Switch to pair with local, monthly climatology of ekman pumping velocity product
   ancilary_data_per_cycle$ekman_velocity_m_d1 <- match_ekman_pumping_velocity_climatology   (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                           "Ancillary data and toolbox/Ancilary data/Ekman pumping velocity", sep="") ,
                                                                                              longitude=ancilary_data_per_cycle$longitude,
                                                                                              latitude=ancilary_data_per_cycle$latitude,
                                                                                              date= ancilary_data_per_cycle$date)
   
   print("Success in ekman pumping velocity matchup")
   
 } # bracket for  "if (Model_setting_list$ekman_velocity==1)"
  
  
  if (Model_setting_list$ekman_pumping_velocity==2){ # Match to real time ekman pumping velocity
    ancilary_data_per_cycle$ekman_velocity_m_d1 <- match_ekman_pumping_velocity_climatology   (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                            "Ancillary data and toolbox/Ancilary data/Ekman pumping velocity", sep="") ,
                                                                                  longitude=ancilary_data_per_cycle$longitude,
                                                                                  latitude=ancilary_data_per_cycle$latitude,
                                                                                  date= ancilary_data_per_cycle$date)
    
    print("Success in ekman pumping velocity matchup")
    
    
  }  
  
  
  
  
  if (Count_No_NA(   ancilary_data_per_cycle$ekman_velocity_m_d1)< 5){
    
    print(   "Warnning: the ekman pumping velocity is set to zero becasue few matchup")
    ancilary_data_per_cycle$ekman_velocity_m_d1=0
   
  } else{
    
    ancilary_data_per_cycle$ekman_velocity_m_d1=(approx(        ancilary_data_per_cycle$date,    
                                                                ancilary_data_per_cycle$ekman_velocity_m_d1,
                                                                ancilary_data_per_cycle$date,
                                                                rule=2)$y)
  }
  
 
  
  # Compute mixed layer  ----------------------------------------------------
  
  
  print("Start computing mixed layer depth")
  
  # Step 1: Determine the mixed layer depth dynamics 
  
  MLD_cycle <- compute_MLD ( cycle=      float_profile_data$cycle,
                             pressure=     float_profile_data$pressure_m,
                             temperature=      float_profile_data $temperature_C,
                             salinity =     float_profile_data$salinity,
                             method=Model_setting_list$MLD_definition  )
  
  # Gap-fill MLD
  
  
  
  ancilary_data_per_cycle=left_join(   ancilary_data_per_cycle,
                                       MLD_cycle,"cycle"   )
  print("Success in mixed layer depth computation")
  
  if (Model_setting_list$integration_depth==1){
    ancilary_data_per_cycle$integration_depth_m=ancilary_data_per_cycle$MLD_m
  }
  
    ancilary_data_per_cycle$integration_depth_m=floor(moving_smooth_fill( ancilary_data_per_cycle$integration_depth_m, 
                                                                        Model_setting_list$data_smooth_number))
    
  #  Moving smooth integration depth -----------------------------------
  
  ancilary_data_per_cycle$integration_depth_m=floor(moving_smooth_fill( ancilary_data_per_cycle$integration_depth_m, 
                                                                        Model_setting_list$data_smooth_number))
  
    
    if (Model_setting_list$integration_depth==1){
      ancilary_data_per_cycle$MLD_m=ancilary_data_per_cycle$integration_depth_m
    }

  # Final step: Merge the ancillary to the float profile data 
  ancilary_data_per_cycle=subset(  ancilary_data_per_cycle,
                                   select=-c(longitude,   
                                             latitude,
                                             date,temperature,salinity,time))
  
  float_profile_data=left_join(  float_profile_data,
                                 ancilary_data_per_cycle,"cycle", keep=F)
  
  float_profile_data=arrange(  float_profile_data,  
                               float_profile_data$cycle, 
                               float_profile_data$pressure_m)
  
  
  
  # Moving smooth the float data 
  if (n_distinct(  float_profile_data$cycle)>10 & n_distinct(  float_profile_data$cycle)>  Model_setting_list$data_smooth_number  ){
    
    # print( "Start in float data moving smooth")
    # float_profile_data=moving_smooth_profile(   float_data=  float_profile_data,
    #                                            smooth_number=Model_setting_list$data_smooth_number   ,
    #                                           col_depth=1,
    #                                          col_profile=3,
    #                                         col_lon=5,
    #                                        col_variable=seq(1,ncol(    float_profile_data),1)
    #)
    
    
    float_profile_data=filter(  float_profile_data,
                                float_profile_data$date>0)
    
    
    
    #   print("Success in float data moving smooth")
    
    
    
    
    # } else{
    #  print("Stop computation since the cycle numbers is too sparse to obtain the reliable NCP estimate.")
  } # loop for  "if (n_distinct(  float_profile_data$cycle)>10"
  
  # only keep the float data at the depth 50m deeper than the deepest integration depth
  # to save the time when matching the depth-resolved product
  
  max_depth= max(float_profile_data$integration_depth_m, 
                 float_profile_data$MLD,na.rm=T)
  float_profile_data <- subset(float_profile_data,
                               
                               float_profile_data$pressure_m<   max_depth+50 
                               )
  
  
  # Step 3: define the productive season and compute median location during the productive period
  
  PS_label<- compute_median_location_productive_season (cycle=float_profile_data$cycle,
                                                        latitiude =  float_profile_data$latitude_N,
                                                        longitude =  float_profile_data$longitude_E,
                                                        date=float_profile_data$date,
                                                        pressure = float_profile_data$pressure_m)
  
  float_profile_data=left_join(   float_profile_data, PS_label, "cycle")
  
  # Exclude the part of float data that lacks the reference location in this year . 
  # This is the case when there is no data falling within the productive season in this year, which prevents us from  
  # sorting the productive season median location for this year )
  
  if (Model_setting_list$background_correction==3){
    
    float_profile_data= subset(float_profile_data,float_profile_data$latitude_median_PS_N> -999)
    
    print("Note: Quasi_eularian background correction approach requires the reference location for each productive season. Some float year data have been excluded, as no data were available during the productive season in these years." )
    
    if( length(float_profile_data$pressure_m)<= 5){
      
      print("Error: the code terminates because of remaining float cycles is less than 5" )
      return()
    }
  }
  
  # Don't apply the background correction
  if ( Model_setting_list$background_correction==1  ){
    float_profile_data$tracer_background_umol_kg=0
    float_profile_data$tracer_background_reference_location_umol_kg =0
    float_profile_data$salinity_background=0
    float_profile_data$salinity_background_reference_location=0
    float_profile_data$temperature_background_C=0
    float_profile_data$temperature_background_reference_location_C=0
    
  } else{
    
    if (Model_setting_list$background_correction==2){ # Lagrangian correction: match to the annual mean background gradient
      
      
      float_profile_data$tracer_background_reference_location_umol_kg =0
      float_profile_data$salinity_background_reference_location=0
      float_profile_data$temperature_background_reference_location_C=0
      
      
      print("Start to collocate with the salinity background product")
      
      float_profile_data$salinity_background <- match_WOA_annual(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                              "Ancillary data and toolbox/Ancilary data/WOA2018/salinity", sep="") ,
                                                                 longitude=float_profile_data$longitude_E,
                                                                 latitude=  float_profile_data$latitude_N,
                                                                 pressure= float_profile_data$pressure_m)
      
      print("Success in salinity background matchup")
      
      print("Start to collocate with the temperature background product")
      
      float_profile_data$temperature_background_C <- match_WOA_annual(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                   "Ancillary data and toolbox/Ancilary data/WOA2018/temperature", sep="") ,
                                                                      longitude=float_profile_data$longitude_E,
                                                                      latitude=  float_profile_data$latitude_N,
                                                                      pressure= float_profile_data$pressure_m)
      
      print("Success in temperature background matchup")
      
      
      # Match to the differnt tracer bakcground gradient 
      if (Model_setting_list$tracer==2){ # NO3 
        print("Start to collocate with the NO3 background product")
        float_profile_data$tracer_background_umol_kg <- match_WOA_annual(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                      "Ancillary data and toolbox/Ancilary data/WOA2018/no3", sep="") ,
                                                                         longitude=  float_profile_data$longitude_E,
                                                                         latitude=  float_profile_data$latitude_N,
                                                                         pressure=  float_profile_data$pressure_m)
        
        print("Success in nitrate background matchup")
      }# bracket for "if (tracer==2)
      
      
      if (Model_setting_list$tracer==1){ # DIC
        print("Start to collocate with the DIC background product")
        
        float_profile_data$tracer_background_umol_kg <-match_DIC_NNGv2LDEO(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                        "Ancillary data and toolbox/Ancilary data/DIC", sep="") ,
                                                                           longitude= float_profile_data$longitude_E,
                                                                           latitude= float_profile_data$latitude_N,
                                                                           pressure=  float_profile_data$pressure_m,
                                                                           date=float_profile_data$date,
                                                                           time_window = "annual")
        
        print("Success in DIC background matchup")
      }# bracket for "if (tracer==2)
      
      
      if (Model_setting_list$tracer==4){ # TA 
        print("Start to collocate with the TA background product")
        float_profile_data$tracer_background_umol_kg <- match_TA_NNGv2LDEO(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                        "Ancillary data and toolbox/Ancilary data/TA", sep="") ,
                                                                                       longitude= float_profile_data$longitude_E,
                                                                                       latitude= float_profile_data$latitude_N,
                                                                                       date=float_profile_data$date,
                                                                                       pressure=  float_profile_data$pressure_m)
        
        
        print("Success in TA background matchup")
      }# bracket for "if (tracer==2)
      
    } # Bracekt for ” if (Model_setting_list$background_correction==2){“
    
  } # Bracket for " if ( Model_setting_list$background_correction==1  |Model_setting_list$tracer==3 | Model_setting_list$tracer==5 )"
  
  
  
  # Background gradient correction method 2:Eularian correction ---------------------------------
  
  
  
  if ( Model_setting_list$background_correction==3  & Model_setting_list$tracer!=3 & Model_setting_list$tracer!=5 ){ # Method 2 for background gradient correction 
    
    
    
    
    print("Start to collocate with the salinity background product")
    
    float_profile_data$salinity_background <- match_WOA_monthly (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                              "Ancillary data and toolbox/Ancilary data/WOA2018/salinity", sep="") ,
                                                                 date=float_profile_data$date,
                                                                 longitude=float_profile_data$longitude_E,
                                                                 latitude=  float_profile_data$latitude_N,
                                                                 pressure= float_profile_data$pressure_m)
    
    float_profile_data$salinity_background_reference_location <- match_WOA_monthly (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                 "Ancillary data and toolbox/Ancilary data/WOA2018/salinity", sep="") ,
                                                                                    date=float_profile_data$date,
                                                                                    longitude=float_profile_data$longitude_median_PS_E,
                                                                                    latitude=  float_profile_data$latitude_median_PS_N,
                                                                                    pressure= float_profile_data$pressure_m)
    
    
    print("Success in salinity background matchup")
    
    print("Start to collocate with the temperature background product")
    
    float_profile_data$temperature_background_C <- match_WOA_monthly(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                  "Ancillary data and toolbox/Ancilary data/WOA2018/temperature", sep="") ,
                                                                     date=float_profile_data$date,
                                                                     longitude=float_profile_data$longitude_E,
                                                                     latitude=  float_profile_data$latitude_N,
                                                                     pressure= float_profile_data$pressure_m)
    
    float_profile_data$temperature_background_reference_location_C <- match_WOA_monthly(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                     "Ancillary data and toolbox/Ancilary data/WOA2018/temperature", sep="") ,
                                                                                        date=float_profile_data$date,
                                                                                        longitude=float_profile_data$longitude_median_PS_E,
                                                                                        latitude=  float_profile_data$latitude_median_PS_N,
                                                                                        pressure= float_profile_data$pressure_m)
    
    print("Success in temperature background matchup")
    
    
    # Match to the different tracer background gradient 
    if (Model_setting_list$tracer==2){ # NO3 
      print("Start to collocate with the NO3 background product")
      float_profile_data$tracer_background_umol_kg <- match_WOA_monthly(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                     "Ancillary data and toolbox/Ancilary data/WOA2018/no3", sep="") ,
                                                                        date=float_profile_data$date,
                                                                        float_profile_data$longitude_E,
                                                                        float_profile_data$latitude_N,
                                                                        float_profile_data$pressure_m)
      
      
      float_profile_data$tracer_background_reference_location_umol_kg <- match_WOA_monthly(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                        "Ancillary data and toolbox/Ancilary data/WOA2018/no3", sep="") ,
                                                                                           date=float_profile_data$date,
                                                                                           float_profile_data$longitude_median_PS_E,
                                                                                           float_profile_data$latitude_median_PS_N,
                                                                                           float_profile_data$pressure_m)
      
      print("Success in nitrate background matchup")
    }# bracket for "if (tracer==2)
    
    
    if (Model_setting_list$tracer==1){ # DIC
      print("Start to collocate with the DIC background product")
      
      float_profile_data$tracer_background_umol_kg <- match_DIC_NNGv2LDEO(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                       "Ancillary data and toolbox/Ancilary data/DIC", sep="") ,
                                                                          longitude= float_profile_data$longitude_E,
                                                                          latitude= float_profile_data$latitude_N,
                                                                          date=float_profile_data$date,
                                                                          pressure=  float_profile_data$pressure_m,
                                                                          time_window = "monthly")
      
      float_profile_data$tracer_background_reference_location_umol_kg <- match_DIC_NNGv2LDEO(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                          "Ancillary data and toolbox/Ancilary data/DIC", sep="") ,
                                                                                             date=float_profile_data$date,
                                                                                             longitude= float_profile_data$longitude_median_PS_E,
                                                                                             latitude=  float_profile_data$latitude_median_PS_N,
                                                                                             pressure=  float_profile_data$pressure_m,
                                                                                             time_window = "monthly")
      
      print("Success in DIC background matchup")
    }# bracket for "if (tracer==2)
    
    
    if (Model_setting_list$tracer==4){ # TA 
      print("Start to collocate with the TA background product")
      float_profile_data$tracer_background_umol_kg <- match_TA_NNGv2LDEO(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                      "Ancillary data and toolbox/Ancilary data/TA", sep="") ,
                                                                         longitude= float_profile_data$longitude_E,
                                                                         date=float_profile_data$date,
                                                                         latitude= float_profile_data$latitude_N,
                                                                         pressure=  float_profile_data$pressure_m,
                                                                         time_window="monthly")
      
      float_profile_data$tracer_background_reference_location_umol_kg <-match_TA_NNGv2LDEO(path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                                                        "Ancillary data and toolbox/Ancilary data/TA", sep="") ,
                                                                                           date=float_profile_data$date,
                                                                                           longitude= float_profile_data$longitude_median_PS_E,
                                                                                           latitude=  float_profile_data$latitude_median_PS_N,
                                                                                           pressure=  float_profile_data$pressure_m,
                                                                                           time_window="monthly")
      
      
      print("Success in TA background matchup")
    }# bracket for "if (tracer==2)
    
    
    
    
    
    
  } # Bracket for "if ( Model_setting_list$background_correction==3  & Model_setting_list$tracer!=3 & Model_setting_list$tracer!=5 )"
  
  # Gap-fill the missing data in the background gradient
  if  (Model_setting_list$tracer_gap_filling==1){
    
    float_profile_data$tracer_background_reference_location_umol_kg= Gap_fill_variable (depth=float_profile_data$pressure_m,
                                                                                        date=float_profile_data$date,
                                                                                        variable=float_profile_data$tracer_background_reference_location_umol_kg)
    
    float_profile_data$tracer_background_umol_kg= Gap_fill_variable (depth=float_profile_data$pressure_m,
                                                                     date=float_profile_data$date,
                                                                     variable=float_profile_data$tracer_background_umol_kg)
    
    
  } # Bracket for if  (Model_setting_list$tracer_gap_filling==1)
  
  
  
  # Set the background value to zero when there is no tracer measurement 
  # float_profile_data$tracer_background_umol_kg[   float_profile_data$tracer_umol_kg1==NaN]=NaN
  #float_profile_data$tracer_background_reference_location_umol_kg[  float_profile_data$tracer_umol_kg1==NaN]=NaN
  #float_profile_data$salinity_background[   float_profile_data$tracer_umol_kg1==NaN]=NaN
  #float_profile_data$salinity_background_reference_location[  float_profile_data$tracer_umol_kg1==NaN]=NaN
  #float_profile_data$temperature_background_C[   float_profile_data$tracer_umol_kg1==NaN]=NaN
  #float_profile_data$temperature_background_reference_location_C[    float_profile_data$tracer_umol_kg1==NaN]=NaN
  
  
  
  return( float_profile_data)
  
  
} # Bracket for  "Match_ancillary_data <- function"
