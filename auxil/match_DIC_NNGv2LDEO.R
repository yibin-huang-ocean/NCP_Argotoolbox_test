
match_DIC_NNGv2LDEO = function ( path, 
                                 longitude  ,
                                 latitude ,
                                 pressure,
                                 date,
                                 time_window) {
  match= rep(NaN,length( longitude))
  month=month(Convert_matlabtime(date))
  setwd(path)
  chla=nc_open('TCO2_NNGv2LDEO_climatology.nc');  #
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
      DIC_profile<-  ncvar_get(chla, "TCO2_NNGv2LDEO",
                               start=c(  month[ia],1,latt,lonn),
                               count= c(1,-1,1,1)) 
      DIC_profile=DIC_profile[1:    depth_index_max]
    }  # Bracket for " if (  time_window=="monthly")"
    
  if (  time_window=="annual"){

    DIC_profile=rep(NaN,   depth_index_max)
    for (i in 1: depth_index_max){
      DIC_profile[i]<-   mean(ncvar_get(chla, "TCO2_NNGv2LDEO",
                                        start=c(  month[ia],i,latt,lonn),
                                        count= c(-1,1,1,1)) ,na.rm=T)
      
     
    }
    
  }  

    
   
   DIC_valid = sum(!(is.na(DIC_profile)))
   
   if (DIC_valid > 3){
     match[ia]<-  approx(  depth[1:depth_index_max],  
                           DIC_profile,
                           pressure[ia] )$y
     
     
     
   } # Bracket for "if (DIC_valid > 3)"
   
   
   print(paste("Progress in DIC background product matchup:",round(ia/length(longitude)*100,3),"%",sep=""))
  }  # Bracket for "for (ia in  1:length(longitude) )"
  
  
  
  return(match)
  
}







