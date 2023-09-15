
match_WOA_monthly <- function ( path, 
                                date ,
                                longitude  ,
                                latitude ,
                                pressure) {
  
  match= rep(NaN,length(date))
  setwd(path)
  file_name=dir()
  # create the file name
  year = month(Convert_matlabtime(date))
  year_unique <- unique (year )
  
 
  # open the annual mean
  annual_file=   grep("00_",   file_name, value = TRUE)
  chla=nc_open(  annual_file);  #
  sensible_annual <-ncvar_get(chla,names (chla$var) [6])
  depth_annual=ncvar_get(chla,"depth")  
  lon_annual=ncvar_get(chla,"lon") 
  lat_annual=ncvar_get(chla,"lat")  
  
  for ( i in 1: length (  year_unique) )  {
    
    # open the monthly climatology
    monthly_label_contain<-  paste(sprintf("%02d",year_unique[i]),"_",sep="")
    monthly_file_name <-  grep( monthly_label_contain,  
                               file_name, 
                               value = TRUE)
    chla=nc_open( monthly_file_name);  #
    
    sensible <-ncvar_get(chla,names (chla$var) [6])
    lon=ncvar_get(chla,"lon")  #更改提取目标变量
    lat=ncvar_get(chla,"lat")  
    lon[lon==179.5]=180
    lon[lon== -179.5]=-180
    time=ncvar_get(chla,"time")  #更改提取目标变量
    depth=ncvar_get(chla,"depth")  #更改提取目标变量
    year_line = which (year== year_unique[i] )
    
    
    ia=1
    for (ia in  year_line ) {
      
      lonn <- max (which ( lon<= longitude [ia]  ))
      latt <- min ( which (lat>= latitude[ia]  ))
      
      
      try({
        
        
        if (pressure[ia]<= max(    depth)){
         
          if (Count_No_NA( sensible[lonn,latt,])>3){
            match[ia]<-  approx(  depth,  
                                  sensible[lonn,latt,],
                                  pressure[ia] )$y
          }
          
          
          
          
          
        } else{ # opt to match to the annual mean because the deepest
          # data of monthly climatology are only 800m
          
          depthh=  min ( which (depth_annual>= pressure[ia]  ))
          
          if (Count_No_NA( sensible[lonn,latt,])>3){
            match[ia]<-  approx(    depth_annual,  
                                    sensible_annual[lonn,latt,],
                                    pressure[ia] )$y
          }
          
          
       
          
          
        }
        
      } ,  silent = FALSE)
      
      
    }
   # print(i/length (  year_unique)*100)
  }
  
#  print( paste( "maximum depth of monthly product:", max(  depth), sep=""))
  return(match)
  
  
}