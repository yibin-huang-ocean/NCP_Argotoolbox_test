match_TA_NNGv2LDEO = function ( path="/Users/yibinhuang/Desktop/Yibin /NASA EXPORT/code/my code/NCP toolbox/Data/Ancilary data/TA_product/", 
                                              longitude = float_profile_data$longitude ,
                                              latitude = float_profile_data$latitude,
                                              pressure=float_profile_data$pressure ,
                                              date=float_profile_data$date,
                                             time_window="annual") {
  match= rep(NaN,length( longitude))
  month=month(Convert_matlabtime(date))
  setwd(path)
  chla=nc_open('AT_NNGv2_climatology.nc');  #
  # calculate the time different with each month
  # sensible <-ncvar_get(chla,"DIC_annual_mean")
  lon=ncvar_get(chla,"longitude")  #更改提取目标变量
  lat=ncvar_get(chla,"latitude")  #更改提目标变量
  time=ncvar_get(chla,"time")  #更改提取目标变量
  depth=ncvar_get(chla,"depth")  #更改提取目标变量
  lon[which.min(  lon)]= -180
  lon[ which.max(  lon)]= 180
  
  lat[ which.min(  lat) ]= -90
  lat[ which.max(  lat)]= 90
  ia=1
  for (ia in  1:length(longitude) ) {
    
    lonn <- max (which ( lon<= longitude [ia]  ))
    latt <- max ( which (lat<= latitude[ia]  ))
    
    if (  time_window=="monthly"){
      DIC_profile<-  ncvar_get(chla, "AT_NNGv2",
                               start=c(  month[ia],1,latt,lonn),
                               count= c(1,-1,1,1)) 
      
    }  
    
    if (  time_window=="annual"){
      
      DIC_profile=rep(NaN,length(depth))
      
      
      for (i in 1:length(depth)){
        DIC_profile[i]<-   mean(ncvar_get(chla, "AT_NNGv2",
                                          start=c(  month[ia],i,latt,lonn),
                                          count= c(-1,1,1,1)) ,na.rm=T)
        
      }
      
    } 
  
    
    DIC_valid = sum(!(is.na(DIC_profile)))
    
    if (DIC_valid > 3){
      match[ia]<-  approx(  depth,  
                            DIC_profile,
                            pressure[ia] )$y
      
      
      
    }
    
    
  }
  
  
  
  return(match)
  
}







