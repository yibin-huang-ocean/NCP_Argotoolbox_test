# Convert the longitude into 0-360°

convert_lon_360= function(lon){
  lon[which(lon<0)]=360+lon[which(lon<0)]
  return(lon)
}
