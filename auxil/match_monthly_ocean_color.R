

match_monthly_ocean_color <- function ( path= paste( Model_setting_list$path_NCP_toolbox,  
                                                     "Ancillary data and toolbox/Ancilary data/Chla", sep="") , 
                                        date=     ancilary_data_per_cycle$date ,
                                        longitude=  ancilary_data_per_cycle$longitude,
                                        latitude=  ancilary_data_per_cycle$latitude  ) {
  
  
  match= rep(NaN,length(date))
  setwd(path)
  file_name_all=dir()
  year = year(Convert_matlabtime(date))
  month=sprintf("%02d",month(Convert_matlabtime(date)))
  file_name <-  paste(  year,  month,sep="")
    
  file_name_unique <- unique (  file_name)
  
  i=1
  for ( i in 1: length (    file_name_unique) )  {
    
    try({
      
      
      # open the file  and extract the variables
      file_name_open <-     grep(  file_name_unique[i],  
                                   file_name_all, 
                                   value = TRUE)
      if (length(   file_name_open)>0){
        chla=nc_open(        file_name_open);
        sensible = ncvar_get(chla, chla$var[[1]])
        lon = ncvar_get(chla,"lon")
        lat = ncvar_get(chla,"lat")
        line = which  ( file_name== file_name_unique [i] )
        
        for (ia in  line) {
          
      
          lonn <- which.min (abs(lon - longitude[ia]))
          latt <- which.min(abs(lat - latitude[ia] ))
          
          
          match[ia] <- (( sensible[lonn,latt])) #
          
          
        }
      }
     
      
      print(   paste("Matching remotely sensed Chla. Progress: ",
                     round(i/length (    file_name_unique)*100,3)
                     
                     ))
      
      
    } ,  silent = FALSE)
  }
  
  

  
  return(match)
  
}