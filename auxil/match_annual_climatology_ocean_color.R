
match_annual_ocean_color <- function ( path, longitude,latitude  ) { # Zeu_lee"
  match= rep(NaN,length( longitude))
  setwd(path)
  
  # open the file  and extract the variables
  chla=nc_open( "annual.nc");
  sensible = ncvar_get(chla, chla$var[[1]])
  lon = ncvar_get(chla,"lon")
  lat = ncvar_get(chla,"lat")
  
  for (i in 1:length (match)) {
    
    lonn <- max (which ( lon<= (longitude[i])   ))
    latt <- max ( which (lat>= latitude[i]  ))
    match[i] <- (( sensible[lonn,latt]))
  }
  
  rm(    sensible)
  
  return(match)
  
}
