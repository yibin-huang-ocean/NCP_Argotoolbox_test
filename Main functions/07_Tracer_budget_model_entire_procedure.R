

Tracer_budget_model_entire_procedure <- function(  
                                Model_setting_list =  Model_setting_list){
  
 
  
 #  Step 1: float data download and process----------------------------------------------------------------
  # Download, load and process (interpolation and moving smooth) the float data 
  # Take a few minutes to run this function, depending on your Internet connection  
  float_profile_data <- download_import_process_float_data(
    Model_setting_list=Model_setting_list
    
  )
  
  
  # Step 2: Collocate the float data with ancillary parameters -----------------------------------------------------------------
  # Take a few minutes to run this function, depending on the tracer and number of float cycles 
  
  if ( is.null(float_profile_data) ){
    return()
   
  }  else{
    float_profile_data_ancillary_data <- Match_ancillary_data (
      float_profile_data=   float_profile_data,
      Model_setting_list =  Model_setting_list
    )
  }
  
  
  # Step 3: Compute the tracer budget with uncertainty simulations---------------------------------------
  if ( is.null(    float_profile_data_ancillary_data)){
    
   return()
  } else{
    tracer_budget<- Perform_tracer_budget(
      float_profile_data=    float_profile_data_ancillary_data,
      Model_setting_list=Model_setting_list
      
    ) 
  }
  
  output=list(tracer_budget, float_profile_data_ancillary_data )
  names(output)[1]="tracer_budget"
  names(output)[2]="float_profile_data"
  return( output)
  
} # Bracket for "Tracer_budget_model"