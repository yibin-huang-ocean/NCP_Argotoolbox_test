tracer_budget_toolbox_settings <- function( 
                              root_path_tracer_budget_toolbox,
                              advanced_setting, 
                              WMOID,   
                              tracer, 
                              integration_depth,
                              background_correction
                              ){
  # Function Description:
  # This function returns a list file containing a set of model settings applied to the tracer budget model
  
  # INPUTS:
  # root_path_tracer_budget_toolbox (character): The root path of the tracer budget toolbox
  # advanced_setting (numeric): 1 for Default settings, 2 for Customization of model settings
  # WMOID (numeric): Float ID in GDAC (only allows to input a single ID)
  # tracer (numeric): # 1. DIC; 2. NO3; 3. Oxygen; 4.TA; 5.POC_bbp
  # integration_depth (numeric): # 1. MLD; 2. varying monthly euphotic zone; 
  #                                3. Fixed depth equal to mean euphotic zone over the float lifetime;
  #                                4. Used-fined fixed depth (must be an integer and greater than 10m). 
  # background_correction (numeric): # 1. NO; 2. Quasi_lagrangian correction (not applicable for oxygen and POC budget); 
                                     #  3. Quasi_eularian correction (not applicable for oxygen and POC budget)
  
  # OUTPUTS:
  # list file: containing all model settings in the tracer budget model
  # Creation DATE: JUNE 1, 2023 (Version 1.0)
  
  
# Default model settings  --------------------------------------------------------

    MLD_defination<- 1
    QC_flag <- 1
    Data_smooth_number <- 5
    Entrainment_parameterization  <- 1
    Ekman_pumping_parameterization  <- 1
    diapyncal_diffusivity  <- 10^-5
    Cycle_filter <- 0
    Iterations_uncertainty_simulation <- 10
    bbp_to_POC_conversion <- 1
    TA_algorithm <- 1
    Gas_model <- 1
    EP_term_computation <- 1
    Car_Equ_Cons_K1_K2 <- 1
    Car_Equ_Cons_Ks <- 1
    Total_boron_concentration <-1
    ekman_pumping_velocity <- 1
    tracer_gap_filling <- 1

# Specify the settings if user chooses to customize the model settings----------------------------------------------------

    if (  advanced_setting!=1){ # Customization of model settings
      
      MLD_defination<- as.numeric(readline("Specify the mixed layer depth defination (Option: 1. Temperature threshold; 2. Density threshold):"))
      Data_smooth_number <- as.numeric(readline("Specify the time step used for moving-smooth (input the number):"))
      diapyncal_diffusivity  <- (readline("Specify the diapyncal diffusivity (backgroudn value: 10^-5 m s-2):"))
      Cycle_filter <- as.vector((readline("Specify the vector indicating cycle nubmers for the float data removal (note 0 represents reataining all cycles):")))
      Cycle_filter <-  eval(parse(text =     Cycle_filter)) # convert into the vector
      
      Iterations_uncertainty_simulation <- as.numeric(readline("Specify the iterations for uncertainty simulation:"))
      tracer_gap_filling <- as.numeric(readline("Specify if gap-fill the tracer missing value (Option: 1. Yes; 2: NO):"))
      ekman_pumping_velocity <- as.numeric(readline("Specify ekman pumping velocity product (Option: 1. real time (online matchup); 2. monthly climatogy (faster matchup)):"))
      

      if (tracer==1){ # DIC budget 
        TA_algorithm <- as.numeric(readline("Specify TA algorithm  (Option: 1. Canyon-B; 2. LIAR (not available in R); 3. Blend of Canyon-B and LIAR  (not available in R)):"))
        Car_Equ_Cons_K1_K2 <- as.numeric(readline("Specify carbonate equilibrium constant of K1 and K2 (Opitioin: 1.Lueker et al. (2000); 2. Millero et al. (2002); 3.Waters et al. (2014); 4. Millero (2010):"))
        Car_Equ_Cons_Ks <- as.numeric(readline("Specify carbonate equilibrium constant of Ks (Opitioin: 1.Dickson (1990); 2. Khoo et al. (1977):"))
        Total_boron_concentration <- as.numeric(readline("Specify concentration of total boron (Opitioin: 1.Lee et al. (2010); 2. Uppstrom (1974) :"))
        Gas_model <- as.numeric(readline("Specifiy the CO2 gas model (Option: 1. Wanninkhof (2014); 2. Wanninkhof (1992); 3. Ho et al., (2006); 4. Sweeney et al. (2007)):"))
        EP_term_computation <- as.numeric(readline("Specifiy the method to account for evaporation and precipitation (Opitioin: 1. Salinity normalizaton (not applicable for O2 budget); 2. Connect to salinity budget (not applicable for O2 budget)):"))
      }
      
      if (tracer==2){ # NO3 budget 
        EP_term_computation <- as.numeric(readline("Specifiy the method to account for evaporation and precipation (Opitioin: 1. Salinity normalizaton (not applicable for O2 budget); 2. Connect to salinity budget (not applicable for O2 budget)):"))
      }
      
      if (tracer==3){ # oxygen budget 
        Gas_model <- as.numeric(readline("Specifiy the O2 gas model (Option: 1. Emerson et al., (2019); 2. Liang et al. (2013); 3. Wanninkhof (2014)):"))
      }
      
      if (tracer==4){ # TA budget 
        TA_algorithm <- as.numeric(readline("Specify TA algorithm  (Option: 1. Canyon-B; 2. LIAR; 3. Blend of Canyon-B and LIAR):"))
        EP_term_computation <- as.numeric(readline("Specifiy the method to account for evaporation and precipitation (Opitioin: 1. Salinity normalizaton (not applicable for O2 budget); 2. Connect to salinity budget (not applicable for O2 budget)):"))
      }
      
   
      if (tracer==5){ # POC_bbp budget
        bbp_to_POC_conversion<- as.numeric(readline("Specify the bbp_700-to-POC algorithm? (Option: 1. Briggs et al., (2011); 2. Johnson et al., (2017);):"))
        EP_term_computation <- as.numeric(readline("Specifiy the method to account for evaporation and precipitation (Opitioin: 1. Salinity normalizaton (not applicable for O2 budget); 2. Connect to salinity budget (not applicable for O2 budget)):"))
      }
      
      
    } # Bracket for if ( advanced_setting!=1)"
    
    

    

# Create the print labels for model settings -------------------------------------------------

    if (tracer==1){
      tracer_print="DIC"
    }
    if (tracer==2){
      tracer_print="NO3"
    }
    if (tracer==3){
      tracer_print="Oxygen"
      background_correction=1
     EP_term_computation=0
    }
    if (tracer==4){
      tracer_print="TA"
    }
    if (tracer==5){
      tracer_print="POC_bbp"
      background_correction=1
    }
    
    
    if (integration_depth==1){
      integration_depth_print="Mixed layer depth"
    }
  
    if (integration_depth==2){
      integration_depth_print="Montlhy varying euphotic zone"
    }
    
    if (integration_depth==3){
      integration_depth_print="Annual mean Euphotic zone"
    }
    
    if (integration_depth>3){
      integration_depth_print=paste ("Fixed depth:",
                                     integration_depth,"m",sep="")
    }
    
    
    
    if ( tracer_gap_filling==1){
      tracer_gap_filling_print="Yes"
    }
    if ( tracer_gap_filling==2){
      tracer_gap_filling_print="No"
    }
    
    if ( ekman_pumping_velocity==1){
      ekman_pumping_velocity_print="real time (online matchup)"
    }
    if ( ekman_pumping_velocity==2){
      ekman_pumping_velocity_print="monthly climatolgy (local matchup)"
    }
   
    
    if (  EP_term_computation == 1){
      EP_term_computation_print <- "salinity normalizaton"
    } 
    if (  EP_term_computation == 2){
      EP_term_computation_print <- "connect to salinity budget"
    }
    
    if (  EP_term_computation == 0){
      EP_term_computation_print <- "Not account for EP"
    }
    
    
  
    if (      bbp_to_POC_conversion == 1){
      bbp_to_POC_conversion_print  <-"Briggs et al., (2011)"
    } else{
      bbp_to_POC_conversion_print  <- "Johnson et al., (2017)"
    }
    
    
    
    if (background_correction == 1){
      background_correction_print <- "No"
    } 
    
    if (background_correction == 2){
      background_correction_print <-"Lagrangian correction"
    } 
    if (background_correction == 3){
      background_correction_print <-"Lagrangian correction"
    } 
    

    
    if (    MLD_defination == 1){
      MLD_defination_print <- "temperature threshold"
    } else{
      MLD_defination_print <- "density threshold"
    }
    
    
    
    
    if (        TA_algorithm == 1){
      TA_algorithm_print <- "Canyon_B"
    } 
    if (        TA_algorithm != 1){
      TA_algorithm=1 
      TA_algorithm_print <- "Canyon_B"
    }
    

    Cycle_filter_print<- "retain all cycles"
    if (      length(Cycle_filter)>=1 &   Cycle_filter[1]!=0 ){
      Cycle_filter_print <-  paste(Cycle_filter)
    } 
    
    
    gas_model_print=NaN
    if (tracer==1){
      
      if (      Gas_model == 1){
        gas_model_print <- "Wanninkholf (2014)"
      } 
      
      if (      Gas_model == 2){
        gas_model_print <- "Wanninkhof (1992)"
      } 
      
      if (      Gas_model == 3){
        gas_model_print <- "Ho et al., (2006)"
      } 
      
      if (      Gas_model == 4){
        gas_model_print <- "Sweeney et al. (2007)"
      } 
    }
    
  
    if (tracer==3){
      
      if (  Gas_model == 1){
        gas_model_print <- "Emerson et al, (2019)"
      } 
    }
  
    Car_Equ_Cons_K1_K2_print=NaN
    if (tracer==1){
      if (    Car_Equ_Cons_K1_K2==1){
        Car_Equ_Cons_K1_K2_print<- "Lueker et al. (2000)"
        
       
      }
      
      if (    Car_Equ_Cons_K1_K2==2){
        Car_Equ_Cons_K1_K2_print<- "Millero et al. (2002)"
        
        
      }
      
      if (    Car_Equ_Cons_K1_K2==3){
        Car_Equ_Cons_K1_K2_print<- "Waters et al. (2014)"
        
        
      }
      
      if (    Car_Equ_Cons_K1_K2==4){
        Car_Equ_Cons_K1_K2_print<- "Millero (2010)"
        
        
      }
      
    } # Bracket for "if (tracer==1)"
    
    
    
    Car_Equ_Cons_Ks_print=NaN
    if (tracer==1){
      if (     Car_Equ_Cons_Ks==1){
        Car_Equ_Cons_Ks_print<- "Dickson (1990)"
        
        
      }
      
      if (   Car_Equ_Cons_Ks==2){
        Car_Equ_Cons_Ks_print<- "Khoo et al. (1977)"
        
        
      }
      
      
    } # Bracket for "if (tracer==1)"
    
    
    
    Total_boron_concentration_print=NaN
    if (tracer==1){
      if (    Total_boron_concentration==1){
        Total_boron_concentration_print<- "Lee et al. (2010)"
        
        
      }
      
      if (    Total_boron_concentration==2){
        Total_boron_concentration_print<- "Uppstrom (1974)"
        
        
      }
      
      
    } # Bracket for "if (tracer==1)"
  

    
    
    diapyncal_diffusivity_print  <-  paste( diapyncal_diffusivity,"s m-2",
                                             sep=)
    
    diapyncal_diffusivity =eval(parse(text =  diapyncal_diffusivity))
    
    
    
    if (tracer==1){ # DIC budget
      bbp_to_POC_conversion_print=NaN
    }
    
    if (tracer==2){# NO3 budget
 
      gas_model_print=NaN
      TA_algorithm_print=NaN
      Car_Equ_Cons_print=NaN
      bbp_to_POC_conversion_print=NaN
    }
    
    if (tracer==3){# oxygen budget
      TA_algorithm_print=NaN
      Car_Equ_Cons_print=NaN
      bbp_to_POC_conversion_print=NaN
    }
    
    if (tracer==4){# TA budget
      gas_model_print=NaN
      bbp_to_POC_conversion_print=NaN
    }
    
    if (tracer==5){# POC budget
      TA_algorithm_print=NaN
      gas_model_print=NaN
      Car_Equ_Cons_print=NaN
      
    }
  

# Print out the model settings ------------------------------------------------------

 Model_setting_print=data.frame(
   parameter=c("WMOID",
   "tracer",
   "integration_depth",
   "background_correction",
   "MLD_defination",
   "data_smooth_number",
   "diapyncal_diffusivity",
   "bbp_to_POC_conversion",
   #  Entrainment_parameterization, 
   "gas_model",
   "iterations_uncertainty_simulation" ,
  "error_assignment",
   "TA_algorithm" ,
   "EP_term_computation",
   "cycle_filter",
   #  Ekman_pumping_parameterization, 

  "gap-fill the tracer missing value",
  "ekman_puming velocity",
  "Car_Equ_Cons_K1_K2",
  "Car_Equ_Cons_Ks",
  "Total_boron_concentration"),
  
  Setting=c(WMOID,
              tracer_print,
              integration_depth_print,
              background_correction_print,
              MLD_defination_print,
              Data_smooth_number,
              diapyncal_diffusivity_print,
            bbp_to_POC_conversion_print,
              #  Entrainment_parameterization, 
              gas_model_print,
              Iterations_uncertainty_simulation ,
              "check the Model_setting_list$error_assignment",
              TA_algorithm_print,
              EP_term_computation_print,
            "check the Model_setting_list$Cycle_filter",
              #  Ekman_pumping_parameterization, 

            tracer_gap_filling_print,
            ekman_pumping_velocity_print,
            Car_Equ_Cons_K1_K2_print,
            Car_Equ_Cons_Ks_print,
            Total_boron_concentration_print
            ))
   

 
  print( Model_setting_print)
 
 
  
  # Create the model settings file ----------------------------------------------
  
  setwd(  root_path_tracer_budget_toolbox)
  error_assignment <- openxlsx::read.xlsx("uncertainty assignment form.xlsx")[,1:3]
  
  # Return the setting for the next step
  
  Model_setting_list=list(
     root_path_tracer_budget_toolbox,
    WMOID,
    tracer,
    integration_depth,
    background_correction,
    MLD_defination,
    Data_smooth_number,
    diapyncal_diffusivity,
    #  Entrainment_parameterization, 
    Gas_model,
    Iterations_uncertainty_simulation ,
    error_assignment,
    TA_algorithm ,
    EP_term_computation,
    Cycle_filter,
    #  Ekman_pumping_parameterization, 
    tracer_gap_filling ,
    bbp_to_POC_conversion,
    ekman_pumping_velocity,
    Car_Equ_Cons_K1_K2,
    Car_Equ_Cons_Ks,
    Total_boron_concentration

  )
  

  
  names( Model_setting_list)=c("path_NCP_toolbox",
                               "WMOID",
                               "tracer",
                               "integration_depth",
                               "background_correction",
                               "MLD_defination",
                               "data_smooth_number", 
                               "diapyncal_diffusivity",
                               #     "entrainment_parameterization",
                               "gas_model",
                               "iterations_uncertainty_simulation",
                               "error_assignment",
                               "TA_algorithm",
                               "EP_term_computation",
                               "cycle_filter",
                               "tracer_gap_filling ",
                               "bbp_to_POC_conversion",
                               "ekman_pumping_velocity",
                               "Car_Equ_Cons_K1_K2",
                               "Car_Equ_Cons_Ks",
                               "Total_boron_concentration"
  )
  
  
  return(Model_setting_list)
  
 
} # Bracket for "tracer_budget_toolbox_settings"
