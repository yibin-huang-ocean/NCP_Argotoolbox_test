
# A integrated function designed to collocate the float 
# data with NCEP product. 
# It will automatically call for the different secondary functions of  NCEP-I (when matching with
# relative humidity) and NCEP-II 

match_NCEP_NCP_toolbox <- function ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                      "Data/Ancilary data/NCEP/vwind", sep="") ,
                           
                         date = float_profile_data$date,
                         longitude= float_profile_data$longitude,
                         latitude= float_profile_data$latitude, 
                        # file_location=1,
                         variable='vwnd.10m',
                         level="gaussian") {
 

  if (variable!='rhum.sig995'){ 
         
         match<- match_NCEP_II_surface  (path= path, 
                                         date= date ,
                                        longitude= longitude,
                                        latitude=   latitude)

  }
       
   if (variable=='rhum.sig995'){  # NCEP file include the humidity across the various levels 
     
     match<- match_NCEP_I_surface (path= path, 
                                   date= date ,
                                   longitude= longitude,
                                   latitude=   latitude)
     
   }


  
  return(match)
  
}