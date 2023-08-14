
match_NCEP_II_surface <- function ( path, date ,longitude,latitude) {
  match= rep(NaN,length(date))
  setwd(path)
  file_name_all=dir()
  year = year(Convert_matlabtime(date))
  year_unique <- unique (year)
  i=1
  for ( i in 1: length (  year_unique) )  {
    file_name_open <-  grep(  year_unique[i],  
                              file_name_all, 
                              value = TRUE)
    # open the file  and extract the variables
    chla=nc_open(  file_name_open);
    sensible <- ncvar_get(chla, names(chla$var)[2])
    lon = ncvar_get(chla,"lon")
    lat = ncvar_get(chla,"lat")
    time = ncvar_get(chla,"time")
    year_line = which (year== year_unique[i] )
    
    for (ia in  year_line ) {
      
      time_difference <- yday(Convert_matlabtime(date[ia]))  # calculate the time different with each month
      if (length(time)>      time_difference){ # Ensure the NCEP product contains the float sampling time period
        
        if (longitude[ia] <=0){
          lonn <- max (which ( lon<= (longitude[ia]+360)   ))
        } else{
          lonn <- max (which ( lon<= (longitude[ia])   ))
        }
        latt <- max ( which (lat>= latitude[ia]  ))
        match[ia] <- mean ( sensible[lonn,latt, time_difference]) # W m-2
        
      } # Bracket for " if (length(time)>      time_difference)"
     
    }
    nc_close(chla)
    rm(    sensible)
  }
  
  return(match)
  
}



