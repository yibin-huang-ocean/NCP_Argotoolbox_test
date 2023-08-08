# Close figures, clean up workspace, clear command window
cat("\014")
rm(list = ls())


# Step 1: Specify the root path of tracer budget toolbox and appkey_Earthda--------------------------
root_path_tracer_budget_toolbox=""
appkey_Earthdata <- 
# copy the APPKEY archived from NASA EARTHDATA website at https://oceandata.sci.gsfc.nasa.gov/appkey/
# This appkey is required to download the remotely sensed chl data from Ocean Color Data 
# Note: appkey_Earthdat may need to be periodically renewed 
# Instructions to obtain the appkey_Earthdata: 
# Step (1): 1. Go to the Earthdata Login page: https://urs.earthdata.nasa.gov/home
# Step (2): If you already have an account, log in with your username and passcode.
# Step (3): If you don't have an account, create a new one by clicking on the "Register" button and providing the required information.
# Step (4): Once you are logged in, go to your profile page (https://urs.earthdata.nasa.gov/profile) and click on the "App keys" tab on the left sidebar.
# Step (5): Click on the "Generate New Key" button to create a new appkey.



# Step 2: Initialize tracer budget toolbox ----------------------------------------
# For the first time running the toolbox, the script will automatically download 
# the ancillary parameters from the Internet. 
# The total file size is ~11Gb, with an estimated time of 20 min to complete the downloading
setwd(paste(  root_path_tracer_budget_toolbox,
              "Main functions",sep="/"))
source("01_Initialize_tracer_budget_toolbox.R")


# Step 3: Specify the tracer budget model setting  --------------------------------------------------
# All the model setting information is stored in the file entitled "Model_setting_list"

Model_setting_list=tracer_budget_toolbox_settings (
  root_path_tracer_budget_toolbox= root_path_tracer_budget_toolbox,
  WMOID=2902753,  # a single float ID; some examples 1: 5905988 (OSP); 2: 5904395 (Southern Ocean); 3. 5904279 (Core float) 4. 2902753 (float without adjusted data)
  tracer=5,        # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
  integration_depth=2, # 1. MLD; 2. varying monthly euphotic zone; 
                       # 3. Fixed depth equal to mean euphotic zone over the float lifetime;
                       # 4. Used-fined fixed depth (must be an integer and greater than 10m). 
  background_correction= 1, # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
                            #  3. Quasi_eularian correction (not applicable for oxygen and POC budget)
  advanced_setting=1        # 1: Default; 2: Customization
)


# Check the error assignment in the uncertainty estimate
Model_setting_list$error_assignment

# Check the cycle number for the removal 
Model_setting_list$cycle_filter


# Step 4: Perform the tracer budget  ------------------------------------

# This function integrates a set of steps  involving in the tracer budget model, including the float data download, processing, ancillary 
# environmental parameters matchup,  tracer budget closure and uncertainty estimate----------------------------------------------------------------
Output<- Tracer_budget_model_entire_procedure  (
  Model_setting_list =  Model_setting_list)

# Extract the output   
tracer_budget=  Output$tracer_budget # time-series of various terms in the tracer budget
float_profile_data=  Output$float_profile_data # depth profile of float data with ancillary environmental parameters
  

# Step 5: Visualization -----------------------------------------------------------

# Plot float trajectory 
plot_float_trajectory(tracer_budget,
                      Model_setting_list)

# Plot time-series tracer profile with integration depth embedded
plot_tracer_profile (float_profile_data,
                     tracer_budget  )

# Plot the tracer budget
plot_tracer_budget( tracer_budget=tracer_budget, 
                    Model_setting_list= Model_setting_list,
                    show_cycle_dot =2, # 1. yes; 2. NO
                    vertical_term_merge= 1) # 1. Yes (merge three vertical terms); 2.No


# Step 6: Export the data and save a txt file -----------------------------------


setwd(paste(  root_path_tracer_budget_toolbox,
              "Output",sep="/"))
file_name=paste(tracer_budget$WMIOD,".txt",sep='')
write.table(tracer_budget,file=file_name[1],
            row.names = F)


























#  Step 4: float data download and process----------------------------------------------------------------
# Download, load and process (interpolation and moving smooth) the float data 
# Take a few minutes to run this function, depending on your Internet connection  
float_profile_data <- download_import_process_float_data(
  Model_setting_list=Model_setting_list
  
)




# Step 5: Collocate the float data with ancillary parameters -----------------------------------------------------------------
# Take a few minutes to run this function, depending on the tracer and number of float cycles 

float_profile_data <- Match_ancillary_data (
  float_profile_data=float_profile_data,
  Model_setting_list =  Model_setting_list
)




# Step 5: Compute the tracer budget with uncertainty simulations---------------------------------------
tracer_budget<- Perform_tracer_budget(
  float_profile_data= float_profile_data,
  Model_setting_list=Model_setting_list
  
) 




