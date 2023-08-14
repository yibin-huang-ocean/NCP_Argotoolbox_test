
# annual WOA

match_WOA_annual = function ( path, longitude  ,latitude ,pressure ) {
  
  match= rep(NaN,length(date))
  setwd(path)
  file_name=dir()
  annual_file=   grep("00_",   file_name, value = TRUE)
  chla=nc_open(  annual_file);  
  # calculate the time different with each month
  sensible <-ncvar_get(chla,names (chla$var) [6])
  lon=ncvar_get(chla,"lon")  #更改提取目标变量
  lat=ncvar_get(chla,"lat")  #更改提目标变量
  time=ncvar_get(chla,"time")  #更改提取目标变量
  depth=ncvar_get(chla,"depth")  #更改提取目标变量
  
  
  ia=1
  for (ia in  1:length(longitude) ) {
    
    lonn <- max (which ( lon<= longitude [ia]  ))
    latt <- min ( which (lat>= latitude[ia]  ))
  # depthh=  min ( which (depth>= pressure[ia]  ))
    
    if (Count_No_NA( sensible[lonn,latt,])>3){
      match[ia]<-  approx(  depth,  
                            sensible[lonn,latt,],
                            pressure[ia] )$y
    }
    
    
    
  
  
  }
  
  
  
  return(match)
  
}

