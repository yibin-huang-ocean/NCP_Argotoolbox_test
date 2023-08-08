

#### moving smooth data 滑动平均

# -------------------------------------------------------------------------
# Moving smooth data
#
# INPUTS:
# -------------------------------------------------------------------------
# data : a vector that is going to be moving smoothed
#  smooth_number : how many data you want to smooth

# OUTPUTS:
# -------------------------------------------------------------------------
# data :  data being moving smoothed
#
# USAGE:
# a = Imoving_smooth (data= argo$tem, smooth_number=4)
#
#
# AUTHOR: Yibin Huang // yibin.huang@duke.edu // 28th April 2020
# -------------------------------------------------------------------------



moving_smooth_fill = function (data,  smooth_number ) {
  library(zoo)
  if (length(data<smooth_number)<smooth_number){
    median=data
  } else{
    median <- rollmean   (  data, smooth_number,fill = "" )
    median[which(is.na(median))]=data[which(is.na(median))]
  }
  return  (median)
  
}

moving_smooth = function (data,  smooth_number ) {
  library(zoo)
  if (length(data<smooth_number)<smooth_number){
    median=data
  } else{
    median <- rollmean   (  data, smooth_number,fill = "" )
  }
  return  (median)
  
}