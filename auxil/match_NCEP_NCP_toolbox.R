
#variable='vwnd.10m', 
#level="gaussian", 

match_NCEP_NCP_toolbox <- function ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                      "Data/Ancilary data/NCEP/vwind", sep="") ,
                           
                         date = float_profile_data$date,
                         longitude= float_profile_data$longitude,
                         latitude= float_profile_data$latitude, 
                        # file_location=1,
                         variable='vwnd.10m',
                         level="gaussian") {
 
   match= rep(NaN,length(date))
   time=Convert_matlabtime( date)
   year= year(Convert_matlabtime(date))
   
    # Extract the file year range 
    setwd( path)
    filenames <- dir()
    file_year<- substring( (  substring(filenames,nchar(filenames)-6)    ),1,4)
    # local matchup 
    local_matchup_line <-  which( year > min(file_year) & year < max(file_year))
     if (length(     local_matchup_line)>0){
       
       if (variable!='rhum.sig995'){ 
         
         match[    local_matchup_line ]<- match_NCEP_II_surface  (path= path, 
                                                                  date= date[   local_matchup_line] ,
                                                                  longitude= longitude[   local_matchup_line],
                                                                  latitude=   latitude[   local_matchup_line])
         
       }
       
       if (variable=='rhum.sig995'){  # NCEP file include the humidity across the various levels 
         
         match[    local_matchup_line ]<- match_NCEP_I_surface(path= path, 
                                                                  date= date[   local_matchup_line] ,
                                                                  longitude= longitude[   local_matchup_line],
                                                                  latitude=   latitude[   local_matchup_line])
         
       }

  
    # Online matchup for the remaining years 
     
    online_matchup_line<-  which( year <= min(file_year) | year >= max(file_year))
     
     if (length(     online_matchup_line)>0 ){
       match[     online_matchup_line]<- NCEP.interp(variable, 
                                                     level, 
                                                     lat=   latitude[     online_matchup_line],
                                                     lon=  longitude[     online_matchup_line], 
                                                     dt= paste(   time[     online_matchup_line], "17:23:12") ,
                                                     interp='linear')
     } # bracket for  " if (length(     online_matchup_line)>0)"
    
     
   } # Bracket for "if (file_location==1)"
  
  
  return(match)
  
}