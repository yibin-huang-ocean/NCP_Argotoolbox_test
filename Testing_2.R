# Script for Testing Advanced Model Settings in the Tracer Budget Toolbox
# Author: Yibin Huang
# Location: Fuzhou
# Date: 10/10/2023

# Note 1:
# Please ensure you are using the most recent version of the toolbox available on my GitHub:
# https://github.com/yibin-huang-ocean/NCP_Argotoolbox_test
# Also, make sure to provide the correct information for "root_path_tracer_budget_toolbox" and "appkey_Earthdata".

# Note 2:
# The toolbox is designed to automatically stop running if the selected float does not meet the tracer budget requirements.
# If this script runs without crashing, it indicates that the toolbox is working well under different settings.






# Close figures, clean up workspace, clear command window
cat("\014")
rm(list = ls())

# Step 1: Specify the root path of tracer budget toolbox and appkey_Earthda--------------------------
root_path_tracer_budget_toolbox<- ""
appkey_Earthdata <- "069271272ca0a0af44726d603a730a4c1c5c6cfe"
# copy the APPKEY archived from NASA EARTHDATA website at https://oceandata.sci.gsfc.nasa.gov/appkey/
# This appkey is required to download the remotely sensed chl data from Ocean Color Data 
# Note: appkey_Earthdat may need to be periodically renewed 
# Instructions to obtain the appkey_Earthdata: 
# Step (1): 1. Go to the Earthdata Login page: https://oceandata.sci.gsfc.nasa.gov/appkey/
# Step (2): If you already have an account, log in with your username and passcode to obtain the APPKEY.
# Step (3): If you don't have an account, create a new one by clicking on the "Register" button and providing the required information.


# Step 2: Initialize tracer budget toolbox ----------------------------------------
# For the first time running the toolbox, the script will automatically download 
# the ancillary parameters from the Internet. 
# The total file size is ~11Gb, with an estimated time of 20 min to complete the downloading
setwd(paste(  root_path_tracer_budget_toolbox,
              "Main functions",sep="/"))
source("01_Initialize_tracer_budget_toolbox.R")



# Compile some representative floats in the global ocean-----------------------------------------------------------------------

float_list_Southern_Ocean= c(5906222,
                             5904182,
                             5904183,
                             5904184,
                             5904185,
                             5904187,
                             5904188,
                             5904469,
                             5904183,
                             5904678,
                             5905072,
                             5906309
)

float_list_North_Pacific= c(5904028,
                            5904033,
                            5906474,
                            5904029,
                            5904030,
                            5906470,
                            5903405,
                            5903891,
                            5904125,
                            5905988,
                            5901468)

float_list_Indian_ocean= c(5906539)



float_list_Atlatntic= c(5903108,
                        5902460,
                        5903106,
                        1902384,
                        5906474,
                        4903365,
                        5904029,
                        5904030,
                        
                        1902383)


float_list_equator= c(5903108,
                      5902460,
                      5903106,
                      4903365,
                      5904029,
                      5906503,
                      
                      5904030)


float_list=c(float_list_Southern_Ocean,
             float_list_equator,
             float_list_North_Pacific,
             float_list_Indian_ocean,
             float_list_Atlatntic)




# Test 1: DIC budget ------------------------------------------------------
# test different options in the gas model, carbonate parameters constant, EP computationand background correction 
i=1
ii=1
iii=1


testing_record=data.frame(WMOID=NaN,
                          tracer=NaN,
                          background_correction=NaN,
                          integration_depth=NaN,
                          ekman_pumping_velocity=NaN,
                          model_run=NaN,
                          model_output=NaN,
                          gas_model=NaN,
                          EP_term=NaN
)



for (i in 1:length(float_list)){
  
    # test the different gas models
    for (ii in 1:4){
      
   
      
    # test 3: the different background correction approaches 
       for (iii in 2:3){
         
         
         Model_setting_list=tracer_budget_toolbox_settings (
           root_path_tracer_budget_toolbox= root_path_tracer_budget_toolbox,
           WMOID=float_list[i],  # a single float ID; some examples 1: 5905988 (OSP); 2: 5906222 (Southern Ocean); 3. 5904279 (Core float) 4. 2902753 (float without adjusted data)
           tracer=1,        # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
           integration_depth=1, # 1. MLD; 
           # 2. varying monthly euphotic zone; 
           # 3. Fixed depth equal to mean euphotic zone over the float lifetime;
           # or user-defined fixed depth (must be an integer and greater than 10m). 
           background_correction= 1, # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
           # 3. Quasi_Eulerian correction (not applicable for oxygen and POC budget)
           ekman_pumping_velocity=2, # 1. Real time (longer waiting time because of the online matchup); 
           # 2. Monthly climatology (short waiting time because of local matchup) 
           advanced_setting=1  # 1: Default; 2: Customization
         )
         
         
         
         
         Model_setting_list$gas_model=ii
         Model_setting_list$background_correction=iii
         
         # the option 1 is default setiing, which has already been tested in another script 
         Model_setting_list$MLD_definition=2
         Model_setting_list$tracer_gap_filling =2
         Model_setting_list$diapycnal_diffusivity=10^-4*1.5
         Model_setting_list$Car_Equ_Cons_Ks=2 
         Model_setting_list$Total_boron_concentration=2
         Model_setting_list$EP_term_computation=2
         Model_setting_list$iterations_uncertainty_simulation=4
         
         # Step 4: Perform the tracer budget  ------------------------------------
         Output<- Tracer_budget_model_entire_procedure  (
           Model_setting_list =  Model_setting_list)
         
         # save the testing information 
         testing_record_1=data.frame(WMOID=Model_setting_list$WMOID,
                                     tracer=Model_setting_list$tracer,
                                     background_correction=Model_setting_list$background_correction,
                                     integration_depth=Model_setting_list$integration_depth,
                                     ekman_pumping_velocity=Model_setting_list$ekman_pumping_velocity,
                                     gas_model=Model_setting_list$gas_model,
                                     EP_term=Model_setting_list$EP_term_computation,
                                     model_run="good",
                                     model_output="no" )
         
         
         if (!is.null(       Output)){
           
           testing_record_1$model_output[1]="yes"
           
           # Extract the output   
           tracer_budget=  Output$tracer_budget # time-series of various terms in the tracer budget
           float_profile_data=  Output$float_profile_data # depth profile of float data with ancillary environmental parameters
           
           
           # Plot float trajectory 
           plot_float_trajectory(tracer_budget,
                                 Model_setting_list)
           
           # Plot time-series tracer profile with integration depth embedded
           plot_tracer_profile (float_profile_data,
                                tracer_budget  )
           
           # Plot the tracer budget
           plot_tracer_budget( tracer_budget=tracer_budget, 
                               Model_setting_list= Model_setting_list,
                               show_cycle_dot =1, # 1. yes; 2. NO
                               vertical_term_merge= 1) # 1. Yes (merge three vertical terms); 2.No
           
           rm( Model_setting_list)
           rm(  Output)
           
         }else{
           testing_record= rbind(  testing_record,testing_record_1)
           rm( Model_setting_list)
           rm(  Output)
         } # loop for " if (!is.null(       Output)){"
         
         
     
         
      } #  loop for iii
    
    } # loop for ii    
    
  
} # loop for "i"



# Test 2: NO3 budget ------------------------------------------------------
# test if the different EP term computations work well under three background correction schemes
i=1
ii=1
iii=1


testing_record=data.frame(WMOID=NaN,
                          tracer=NaN,
                          background_correction=NaN,
                          integration_depth=NaN,
                          ekman_pumping_velocity=NaN,
                          model_run=NaN,
                          model_output=NaN,
                          gas_model=NaN,
                          EP_term=NaN
)



for (i in 1:length(float_list)){

    # test 3: the different background correction approach 
    for (iii in 1:3){
      
      
      Model_setting_list=tracer_budget_toolbox_settings (
        root_path_tracer_budget_toolbox= root_path_tracer_budget_toolbox,
        WMOID=float_list[i],  # a single float ID; some examples 1: 5905988 (OSP); 2: 5906222 (Southern Ocean); 3. 5904279 (Core float) 4. 2902753 (float without adjusted data)
        tracer=2,        # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
        integration_depth=1, # 1. MLD; 
        # 2. varying monthly euphotic zone; 
        # 3. Fixed depth equal to mean euphotic zone over the float lifetime;
        # or user-defined fixed depth (must be an integer and greater than 10m). 
        background_correction= 1, # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
        # 3. Quasi_Eulerian correction (not applicable for oxygen and POC budget)
        ekman_pumping_velocity=2, # 1. Real time (longer waiting time because of the online matchup); 
        # 2. Monthly climatology (short waiting time because of local matchup) 
        advanced_setting=1  # 1: Default; 2: Customization
      )
      
      
      Model_setting_list$background_correction=iii
      
      # the option 1 is default settiing, which has already been tested in another script 
      Model_setting_list$MLD_definition=2
      Model_setting_list$tracer_gap_filling =2
      Model_setting_list$diapycnal_diffusivity=10^-4*1.5
      Model_setting_list$EP_term_computation=2
      Model_setting_list$iterations_uncertainty_simulation=4
      # Step 4: Perform the tracer budget  ------------------------------------
      Output<- Tracer_budget_model_entire_procedure  (
        Model_setting_list =  Model_setting_list)
      
      # save the testing information 
      testing_record_1=data.frame(WMOID=Model_setting_list$WMOID,
                                  tracer=Model_setting_list$tracer,
                                  background_correction=Model_setting_list$background_correction,
                                  integration_depth=Model_setting_list$integration_depth,
                                  ekman_pumping_velocity=Model_setting_list$ekman_pumping_velocity,
                                  gas_model=Model_setting_list$gas_model,
                                  EP_term=Model_setting_list$EP_term_computation,
                                  model_run="good",
                                  model_output="no" )
      
      
      if (!is.null(       Output)){
        
        testing_record_1$model_output[1]="yes"
        
        # Extract the output   
        tracer_budget=  Output$tracer_budget # time-series of various terms in the tracer budget
        float_profile_data=  Output$float_profile_data # depth profile of float data with ancillary environmental parameters
        
        
        # Plot float trajectory 
        plot_float_trajectory(tracer_budget,
                              Model_setting_list)
        
        # Plot time-series tracer profile with integration depth embedded
        plot_tracer_profile (float_profile_data,
                             tracer_budget  )
        
        # Plot the tracer budget
        plot_tracer_budget( tracer_budget=tracer_budget, 
                            Model_setting_list= Model_setting_list,
                            show_cycle_dot =1, # 1. yes; 2. NO
                            vertical_term_merge= 1) # 1. Yes (merge three vertical terms); 2.No
        
        rm( Model_setting_list)
        rm(  Output)
        
      }else{
        testing_record= rbind(  testing_record,testing_record_1)
        rm( Model_setting_list)
        rm(  Output)
      } # loop for " if (!is.null(       Output)){"
      
      
      
      
    } #  loop for iii
  


} # loop for "i"





# Test 3: O2 budget ------------------------------------------------------
# only alter O2 gas model
i=1
ii=1
iii=1


testing_record=data.frame(WMOID=NaN,
                          tracer=NaN,
                          background_correction=NaN,
                          integration_depth=NaN,
                          ekman_pumping_velocity=NaN,
                          model_run=NaN,
                          model_output=NaN,
                          gas_model=NaN,
                          EP_term=NaN
)



for (i in 1:length(float_list)){
  
  # test 3: the different O2 gas model
  for (iii in 1:3){
    
    
    Model_setting_list=tracer_budget_toolbox_settings (
      root_path_tracer_budget_toolbox= root_path_tracer_budget_toolbox,
      WMOID=float_list[i],  # a single float ID; some examples 1: 5905988 (OSP); 2: 5906222 (Southern Ocean); 3. 5904279 (Core float) 4. 2902753 (float without adjusted data)
      tracer=3,        # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
      integration_depth=1, # 1. MLD; 
      # 2. varying monthly euphotic zone; 
      # 3. Fixed depth equal to mean euphotic zone over the float lifetime;
      # or user-defined fixed depth (must be an integer and greater than 10m). 
      background_correction= 1, # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
      # 3. Quasi_Eulerian correction (not applicable for oxygen and POC budget)
      ekman_pumping_velocity=2, # 1. Real time (longer waiting time because of the online matchup); 
      # 2. Monthly climatology (short waiting time because of local matchup) 
      advanced_setting=1  # 1: Default; 2: Customization
    )
    
    
    Model_setting_list$gas_model=iii
    
    # the option 1 is defualt  settiing, which has already been tested in another script 
    Model_setting_list$MLD_definition=2
    Model_setting_list$iterations_uncertainty_simulation=4
    Model_setting_list$tracer_gap_filling =2
    Model_setting_list$diapycnal_diffusivity=10^-4*1.5

    # Step 4: Perform the tracer budget  ------------------------------------
    Output<- Tracer_budget_model_entire_procedure  (
      Model_setting_list =  Model_setting_list)
    
    # save the testing information 
    testing_record_1=data.frame(WMOID=Model_setting_list$WMOID,
                                tracer=Model_setting_list$tracer,
                                background_correction=Model_setting_list$background_correction,
                                integration_depth=Model_setting_list$integration_depth,
                                ekman_pumping_velocity=Model_setting_list$ekman_pumping_velocity,
                                gas_model=Model_setting_list$gas_model,
                                EP_term=Model_setting_list$EP_term_computation,
                                model_run="good",
                                model_output="no" )
    
    
    if (!is.null(       Output)){
      
      testing_record_1$model_output[1]="yes"
      
      # Extract the output   
      tracer_budget=  Output$tracer_budget # time-series of various terms in the tracer budget
      float_profile_data=  Output$float_profile_data # depth profile of float data with ancillary environmental parameters
      
      
      # Plot float trajectory 
      plot_float_trajectory(tracer_budget,
                            Model_setting_list)
      
      # Plot time-series tracer profile with integration depth embedded
      plot_tracer_profile (float_profile_data,
                           tracer_budget  )
      
      # Plot the tracer budget
      plot_tracer_budget( tracer_budget=tracer_budget, 
                          Model_setting_list= Model_setting_list,
                          show_cycle_dot =1, # 1. yes; 2. NO
                          vertical_term_merge= 1) # 1. Yes (merge three vertical terms); 2.No
      
      rm( Model_setting_list)
      rm(  Output)
      
    }else{
      testing_record= rbind(  testing_record,testing_record_1)
      rm( Model_setting_list)
      rm(  Output)
    } # loop for " if (!is.null(       Output)){"
    
    
    
    
  } #  loop for iii
  
  
  
} # loop for "i"






# Test 4: TA budget ------------------------------------------------------
# only alter EP term parameterization 
i=1
ii=1
iii=1


testing_record=data.frame(WMOID=NaN,
                          tracer=NaN,
                          background_correction=NaN,
                          integration_depth=NaN,
                          ekman_pumping_velocity=NaN,
                          model_run=NaN,
                          model_output=NaN,
                          gas_model=NaN,
                          EP_term=NaN
)



for (i in 1:length(float_list)){
  
  # test 3: different background correction 
  for (iii in 1:3){
    
    
    Model_setting_list=tracer_budget_toolbox_settings (
      root_path_tracer_budget_toolbox= root_path_tracer_budget_toolbox,
      WMOID=float_list[i],  # a single float ID; some examples 1: 5905988 (OSP); 2: 5906222 (Southern Ocean); 3. 5904279 (Core float) 4. 2902753 (float without adjusted data)
      tracer=4,        # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
      integration_depth=1, # 1. MLD; 
      # 2. varying monthly euphotic zone; 
      # 3. Fixed depth equal to mean euphotic zone over the float lifetime;
      # or user-defined fixed depth (must be an integer and greater than 10m). 
      background_correction= 1, # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
      # 3. Quasi_Eulerian correction (not applicable for oxygen and POC budget)
      ekman_pumping_velocity=2, # 1. Real time (longer waiting time because of the online matchup); 
      # 2. Monthly climatology (short waiting time because of local matchup) 
      advanced_setting=1  # 1: Default; 2: Customization
    )
    
    
    Model_setting_list$background_correction=iii
    
    # the option 1 is default settiing, which has already been tested in another script 
    Model_setting_list$MLD_definition=2
    Model_setting_list$iterations_uncertainty_simulation=4
    Model_setting_list$tracer_gap_filling =2
    Model_setting_list$diapycnal_diffusivity=10^-4*1.5
    Model_setting_list$EP_term_computation=2
    
    # Step 4: Perform the tracer budget  ------------------------------------
    Output<- Tracer_budget_model_entire_procedure  (
      Model_setting_list =  Model_setting_list)
    
    # save the testing information 
    testing_record_1=data.frame(WMOID=Model_setting_list$WMOID,
                                tracer=Model_setting_list$tracer,
                                background_correction=Model_setting_list$background_correction,
                                integration_depth=Model_setting_list$integration_depth,
                                ekman_pumping_velocity=Model_setting_list$ekman_pumping_velocity,
                                gas_model=Model_setting_list$gas_model,
                                EP_term=Model_setting_list$EP_term_computation,
                                model_run="good",
                                model_output="no" )
    
    
    if (!is.null(       Output)){
      
      testing_record_1$model_output[1]="yes"
      
      # Extract the output   
      tracer_budget=  Output$tracer_budget # time-series of various terms in the tracer budget
      float_profile_data=  Output$float_profile_data # depth profile of float data with ancillary environmental parameters
      
      
      # Plot float trajectory 
      plot_float_trajectory(tracer_budget,
                            Model_setting_list)
      
      # Plot time-series tracer profile with integration depth embedded
      plot_tracer_profile (float_profile_data,
                           tracer_budget  )
      
      # Plot the tracer budget
      plot_tracer_budget( tracer_budget=tracer_budget, 
                          Model_setting_list= Model_setting_list,
                          show_cycle_dot =1, # 1. yes; 2. NO
                          vertical_term_merge= 1) # 1. Yes (merge three vertical terms); 2.No
      
      rm( Model_setting_list)
      rm(  Output)
      
    }else{
      testing_record= rbind(  testing_record,testing_record_1)
      rm( Model_setting_list)
      rm(  Output)
    } # loop for " if (!is.null(       Output)){"
    
    
    
    
  } #  loop for iii
  
  
  
} # loop for "i"



# Test 5: POC budget ------------------------------------------------------
#  alter bpp-POC conversion and EP term computation 
i=1
ii=1
iii=1


testing_record=data.frame(WMOID=NaN,
                          tracer=NaN,
                          background_correction=NaN,
                          integration_depth=NaN,
                          ekman_pumping_velocity=NaN,
                          model_run=NaN,
                          model_output=NaN,
                          gas_model=NaN,
                          EP_term=NaN
)



for (i in 1:length(float_list)){
  
 
    Model_setting_list=tracer_budget_toolbox_settings (
      root_path_tracer_budget_toolbox= root_path_tracer_budget_toolbox,
      WMOID=float_list[i],  # a single float ID; some examples 1: 5905988 (OSP); 2: 5906222 (Southern Ocean); 3. 5904279 (Core float) 4. 2902753 (float without adjusted data)
      tracer=5,        # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
      integration_depth=1, # 1. MLD; 
      # 2. varying monthly euphotic zone; 
      # 3. Fixed depth equal to mean euphotic zone over the float lifetime;
      # or user-defined fixed depth (must be an integer and greater than 10m). 
      background_correction= 1, # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
      # 3. Quasi_Eulerian correction (not applicable for oxygen and POC budget)
      ekman_pumping_velocity=2, # 1. Real time (longer waiting time because of the online matchup); 
      # 2. Monthly climatology (short waiting time because of local matchup) 
      advanced_setting=1  # 1: Default; 2: Customization
    )
    
    

    
    # the option 1 is default setting, which has already been tested in another script 
    Model_setting_list$MLD_definition=2
    Model_setting_list$iterations_uncertainty_simulation=4
    Model_setting_list$tracer_gap_filling =2
    Model_setting_list$diapycnal_diffusivity=10^-4*1.5
    Model_setting_list$EP_term_computation=2
    Model_setting_list$bbp_to_POC_conversion=2

      # Step 4: Perform the tracer budget  ------------------------------------
    Output<- Tracer_budget_model_entire_procedure  (
      Model_setting_list =  Model_setting_list)
    
    # save the testing information 
    testing_record_1=data.frame(WMOID=Model_setting_list$WMOID,
                                tracer=Model_setting_list$tracer,
                                background_correction=Model_setting_list$background_correction,
                                integration_depth=Model_setting_list$integration_depth,
                                ekman_pumping_velocity=Model_setting_list$ekman_pumping_velocity,
                                gas_model=Model_setting_list$gas_model,
                                EP_term=Model_setting_list$EP_term_computation,
                                model_run="good",
                                model_output="no" )
    
    
    if (!is.null(       Output)){
      
      testing_record_1$model_output[1]="yes"
      
      # Extract the output   
      tracer_budget=  Output$tracer_budget # time-series of various terms in the tracer budget
      float_profile_data=  Output$float_profile_data # depth profile of float data with ancillary environmental parameters
      
      
      # Plot float trajectory 
      plot_float_trajectory(tracer_budget,
                            Model_setting_list)
      
      # Plot time-series tracer profile with integration depth embedded
      plot_tracer_profile (float_profile_data,
                           tracer_budget  )
      
      # Plot the tracer budget
      plot_tracer_budget( tracer_budget=tracer_budget, 
                          Model_setting_list= Model_setting_list,
                          show_cycle_dot =1, # 1. yes; 2. NO
                          vertical_term_merge= 1) # 1. Yes (merge three vertical terms); 2.No
      
      rm( Model_setting_list)
      rm(  Output)
      
    }else{
      testing_record= rbind(  testing_record,testing_record_1)
      rm( Model_setting_list)
      rm(  Output)
    } # loop for " if (!is.null(       Output)){"
    

  
  
} # loop for "i"


