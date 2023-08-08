# Moving smooth the profile
# data need to be interpolated to the depth at every 1m
# the data should be mannually reorder after using the function
# Author: Yibin Huang | 2021/02/14

moving_smooth_profile=function(   float_data ,
                                  smooth_number ,
                                  col_depth,
                                  col_profile,
                                  col_lon,
                                  col_variable
){
  
  if (  max(float_data[,col_lon] ) - min(float_data[,col_lon]) >  340 ){
    float_data[,col_lon]=convert_lon_360(float_data[,col_lon])
  }
  
  
  # function to smooth one layer
  smooth_one_layer=function(one_layer){
    one_layer= arrange( one_layer,  one_layer[,   col_profile]    )
    one_layer=rollapply(one_layer, smooth_number, mean)
    return(    one_layer)
  }
  
  smoothdata=ddply(  float_data, .(pressure), smooth_one_layer)
  
  # 滑动平均后，重新排序数据
  smoothdata= arrange(  smoothdata,  smoothdata[,     col_profile],  smoothdata[,     col_depth] )
  
  #  smoothdata$time <- Convert_matlabtime (   smoothdata$date )
  
  
  smoothdata[,col_lon]=convert_lon_180(lon=smoothdata[,col_lon])
  
  return(  smoothdata)
  
  
}

