# This function is to match the ancillary data 
# alongside the float trajectory archived from ERDDAP

match_parameter_ERDAPP_surface=function(
                                date=     ancilary_data_per_cycle$date ,
                                longitude=  ancilary_data_per_cycle$longitude,
                                latitude=  ancilary_data_per_cycle$latitude,
                                dataset="erdQMstress1day",
                                url="https://coastwatch.pfeg.noaa.gov/erddap/",
                                parameter_name="upwelling",
                              #  pressure= float_profile_data$pressure_m,
                                zcoord_included="yes",
                                zcoord_level=0,
                                zcoord_name="altitude"){
  iii=1
  parameter_match= rep(NaN,length(date))
  dataInfo <- rerddap::info(dataset, url)
  dataInfo
  # 步骤二：设置提取数据
  parameter <-     parameter_name
  i=1
  for (i in 1:length(date)){
    
      xcoord <-  longitude[i]
      ycoord <-     latitude[i]
      tcoord<- Convert_matlabtime( date[i])
      zcoord <-   zcoord_level
      
      if (     zcoord_included=="no"){
        swchl <- rxtracto(dataInfo, 
                          parameter=parameter, 
                          xcoord=xcoord, 
                          xName="longitude",
                          
                          ycoord=ycoord, 
                          yName="latitude",
                          
                          tcoord=tcoord,
                          tName="time"
                          
                          #  zcoord=zcoord,
                          #  zName="LEV"
        )
      } 
      
      if (  zcoord_included!="no"){
        swchl <- rxtracto(dataInfo, 
                          parameter=parameter, 
                          
                          xcoord=xcoord, 
                          xName="longitude",
                          
                          ycoord=ycoord, 
                          yName="latitude",
                          
                          tcoord=tcoord,
                          tName="time",
                          
                          zcoord=zcoord,
                          zName= zcoord_name)
      }
   

      parameter_match[i]= swchl[[1]]
      if (dataset=="erdQMstress1day"){
        print(paste("matching the ekman pumping velocity archived from ERDDAP. Progress:", round(i/length(  parameter_match)*100,3),"%"))
      }
      
      if (dataset=="hawaii_soest_034c_01ca_73a8"){
        print(paste("matching the surface chl archived from  ERDDAP. Progress:", round(i/length(  parameter_match)*100,3),"%"))
      }
     
    } 
      
    return(      parameter_match)
}






