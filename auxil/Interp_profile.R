

####将argo的垂直剖面插值 每米

# -------------------------------------------------------------------------
# Interpolate the profile into the every meter
#
# INPUTS:
# -------------------------------------------------------------------------
# float_data : file for the data with the first column being pressure and one of the columns being profile
# column_depth : depth column
# column_profile : profile column
# column_variable_start: column for the first variable (including the profile)
# column_variable_end: column for the last variable
# min_depth : depth starting interpolation (m)
# max_depth : depth ending interpolation (m)
# interval : depth interval for the interpolation (m)

# OUTPUTS:
# -------------------------------------------------------------------------
# float_data2 :  data being interpolated
#
# USAGE:
# need to define  the function of Count_No_NA
# a = Interp_profile  (float_data, column_depth=1, column_profile=4, column_variable_start=2,  column_variable_end=11, min_depth=5,  max_depth=200, interval=1 )
#
#
# AUTHOR: Yibin Huang // yibin.huang@duke.edu // 27th April 2020
# -------------------------------------------------------------------------
#
# USAGE:
# need to define  the function of Count_No_NA
# a = Interp_profile  (float_data, column_depth=1, column_profile=4, column_variable_start=2,  column_variable_end=11, min_depth=5,  max_depth=200, interval=1 )
#
#
# AUTHOR: Yibin Huang // yibin.huang@duke.edu // 27th April 2020
# -------------------------------------------------------------------------

Interp_profile = function (float_data, 
                           column_depth, 
                           column_profile,
                           column_variable_start,  
                           column_variable_end, 
                           min_depth,  
                           interval ) {
  
  float_data=arrange(float_data,   float_data[,column_profile],float_data[,column_depth] )
  float_data=subset( float_data, 
                     float_data[,column_depth]>min_depth )
  profile_unique <- unique(float_data [,column_profile] ) # extract the profile information
  my_list=vector("list",length(  profile_unique ))  # create the list to store the date
  
  # interpolate the first profile
  
  a=filter(float_data,float_data[,column_profile] == profile_unique[1] )
  a <- a [!duplicated (a[,column_depth]), ]  # Make sure the depth information different to avoid the error
  
  # adjust the interpolation depth based on the each profile sampling 
  # depth 
  max_depth_real=max(a[,column_depth]) 
  depthh=seq( min_depth,       max_depth_real, interval)  # 生成实际的插值的深度序列
  
 float_data1=data.frame (press =   depthh)  # create the data frame to store the date
  
  
  for (i in (column_variable_start+1): column_variable_end ) {
    if (   Count_No_NA (a[,i])  > 4 &   min (a[,column_depth],na.rm = T) <=10   )  { ### 目标差值的列的NA的数目必须大于4 且10m以浅有数据
      if (       Count_No_NA (a[ which(  a[,column_depth]<=10 )  ,i] )   >0   ){
        
        
        variable_target = approx ( a[,column_depth],a[,i], depthh,rule=2 )$y
        
        float_data1= cbind ( float_data1, variable_target )
      } else{
        float_data1= cbind ( float_data1, NaN )
      }
      
    } else{
      float_data1= cbind ( float_data1, NaN )
    }
  }
  
  
  column_name <-  colnames ( float_data)[column_variable_start: column_variable_end]  # extract the column  names
  colnames ( float_data1 ) <- column_name  # name the column
  
  
  my_list[[1]]=float_data1
  # interpolate the other profiles
  if (length ( unique(float_data [,column_profile] ))>1){
   
    profilenumber <- length ( unique(float_data [,column_profile] ))
    
    for (k in 2: profilenumber ) {
      a <- filter ( float_data, float_data [,column_profile] == profile_unique[k] )
      a <- a [!duplicated (a[,column_depth]), ]  # Make sure the depth information different to avoid the error
      
      # adjust the interpolation depth based on the each profile sampling 
      # depth 
      max_depth_real=max(a[,column_depth]) 
      depthh=seq( min_depth,       max_depth_real, interval)  # 生成插值的深度序列
      
      float_data2=data.frame (press = depthh)  # create the dataframe to store the date
      for (i in (column_variable_start+1): column_variable_end ) {
        if (    Count_No_NA (a[,i])  > 4 &   min (a[,column_depth],na.rm = T) <=10  )  { ### 确保10m以浅有数据，而且剖面的的非NA的个数大于4个
          if (       Count_No_NA (a[ which(  a[,column_depth]<=10 )  ,i] )   >0   ){
            variable_target = approx ( a[,column_depth],a[,i], depthh,rule=2 )$y
            
            float_data2= cbind ( float_data2, variable_target )
          } else{
            float_data2= cbind ( float_data2, NaN )
          }
        }else{
          float_data2= cbind ( float_data2, NaN )
          
        }
      }
      
      column_name <-  colnames ( float_data)[column_variable_start: column_variable_end] # extract the column  names
      colnames ( float_data2 ) <- column_name  # name the column
      
      
      my_list[[k]]=float_data2
      
    }
  }
  
  
  # 合并所有剖面
  float_data1 <-  bind_rows(   my_list)
  rm(my_list)
  
  float_data1=arrange( float_data1, float_data1[,column_profile],  float_data1[,  column_depth]  )
  
  
  float_data1= filter (float_data1,  float_data1[,column_profile]> -999)  # 提出剖面编号的NA的数据
  
  return (float_data1)
  
}