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
  lon=ncvar_get(chla,"longitude")  
  lat=ncvar_get(chla,"latitude") 
  time=ncvar_get(chla,"time")  
  depth=ncvar_get(chla,"depth") 
  lon[which.min(  lon)]= -180
  lon[ which.max(  lon)]= 180
  
  lat[ which.min(  lat) ]= -90
  lat[ which.max(  lat)]= 90
  ia=1
  for (ia in  1:length(longitude) ) {
    
    lonn <- max (which ( lon<= longitude [ia]  ))
    latt <- max ( which (lat<= latitude[ia]  ))
    depth_index_max= which.min(pressure[ia] >depth)+3
    depth_index_max[    depth_index_max>length(depth)]=depth_index_max>length(depth)
    
    if (  time_window=="monthly"){
      TA_profile<-  ncvar_get(chla, "AT_NNGv2",
                               start=c(  month[ia],1,latt,lonn),
                               count= c(1,-1,1,1)) 
      TA_profile<-  TA_profile[1: depth_index_max]
    }  
    
    if (  time_window=="annual"){
      
    
     TA_profile=rep(NaN,   depth_index_max)
      
      
      for (i in 1: depth_index_max){
        TA_profile[i]<-   mean(ncvar_get(chla, "AT_NNGv2",
                                          start=c(  month[ia],i,latt,lonn),
                                          count= c(-1,1,1,1)) ,na.rm=T)
        
      }
      
    } 
  
    
 TA_valid = sum(!(is.na(TA_profile)))
    
    if (TA_valid > 3){
      match[ia]<-    approx(  depth[1:depth_index_max], 
                        TA_profile,
                            pressure[ia] )$y
      
      
      
    }
    
    print(paste("Progress in TA background product matchup:",round(ia/length(longitude)*100,3),"%",sep=""))
  }
  
  
  
  return(match)
  
}







