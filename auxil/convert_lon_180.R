
convert_lon_180= function(lon){
  lon[which(lon>180)]=lon[which(lon>180)]-360
  return(lon)
}