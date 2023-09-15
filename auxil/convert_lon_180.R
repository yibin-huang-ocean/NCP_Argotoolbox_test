# convert the longitude into the range between 
# -180 and 180 
convert_lon_180= function(lon){
  lon[which(lon>180)]=lon[which(lon>180)]-360
  return(lon)
}