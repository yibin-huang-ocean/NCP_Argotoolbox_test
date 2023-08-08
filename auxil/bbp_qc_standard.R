
# Quality control for float measured bbp_700 data 
# Reference: https://doi.org/10.13155/60262
# Created by Yibin Huang
# Creation date: October 19th, 2022
# Step 1:
# 1.	start from the raw bbp_700 derived from the "Sprof" file 
# 2.	Remove bbp_700 out of the realistic range  (<−0.000025 or >0.1)

#  Step 2: Remove the negative spike 
#  1. Compute the noise for each profile: RES = V2 - median(V0,V1,V2,V3,V4)
#  2. Estimate the 10% percentile RES for each profile：percentile10(RES)
#  3. discard the bbp data with RES smaller than 2*percentile10(RES) （RES< 2*percentile10(RES)）

#   Step 3: get rid of Bad dark offset test
#   1. 5-point median filter: BBP700_smooth = median_filter(5, BBP700)
#   2. If min(BBP700_smooth) < -0.000025 m-1， QC_THRESHOLD_700 = -20*min(BBP700_smooth) 
#   3.  如果  BBP700 < QC_THRESHOLD_700 ，设置NaN 


bbp_qc_standard=function( cycle=float_profile_data$CYCLE_NUMBER,
                          pressure=float_profile_data$PRES,
                          bbp_raw= float_profile_data$BBP700){ #1
  
  # Step 1 : Import float --------------------------------------------------
  
    float_data<- data.frame(cycle=cycle,
                               pressure=        pressure,
                               bbp_raw=      bbp_raw)   
    float_data_original <-  float_data # 保留原始剖面，用于最后的merge
    
    
    if ( all(is.na( float_data$bbp_raw))  )  {   #2 如果不存在bbp，就不执行质控
      
      float_data_original=float_data
    } #2
    
  
    if ( !all(is.na(float_data$bbp_raw))  )  { #3 只有存在bbp的数据的时候开始质控
      
      # Step 2 : Remove the data out of realistic range--------------------------------------------------
      
      float_data=subset(float_data,
                        float_data$bbp_raw > -0.000025 &
                          float_data$bbp_raw <0.1)
      
      
      # Step 2 : apply the filter and calculate the noise for each profile --------------------------------------------------
      cycle_label=unique(float_data$cycle)
      profile_list <- vector("list", length(cycle_label))
      float_data=arrange(float_data,float_data$cycle,
                         float_data$pressure)
      i=1
      for (i in 1: length(cycle_label)){ #4
        
        float_profile=filter(float_data,
                             float_data$cycle==  cycle_label[i]) # 提取每个剖面
        float_profile=arrange(    float_profile,    
                                  float_profile$pressure)
        
        # Step 2.1 : calculate the Res (RES = V2 - median(V0,V1,V2,V3,V4) -------------------------------------------------
        if (Count_No_NA ( float_profile$bbp_raw)>11){#5
          
          # compute the 5-point running median value 
          
          float_profile=arrange(    float_profile,    
                                    float_profile$pressure)
          
          float_profile$bbp_median_5_point=rollmedian(float_profile$bbp_raw,
                                                      5,fill = "extend",na.pad = TRUE) # step 1: 找出spike
          
          float_profile$RES=    float_profile$bbp_raw-       float_profile$bbp_median_5_point
          
          threshold_negative_spike=  quantile(     float_profile$RES,0.1 ,na.rm=T)*2
          
          # remove the negative spike based on the threshold 
          float_profile=subset(float_profile,
                               float_profile$RES> threshold_negative_spike |
                                 float_profile$pressure<11
          )
          
          
          # Step 3 : remove the dark offest data-------------------------------------------------
          
          #  5 point rolling median fitler 
          
          float_profile=arrange(    float_profile,    
                                    float_profile$pressure)
          
          float_profile$BBP700_smooth=rollmedian(float_profile$bbp_raw,
                                                 5,fill = "extend",na.pad = TRUE)
          
          if ( min (  float_profile$BBP700_smooth, na.rm = T)<  -0.000025){
            
            QC_THRESHOLD_700 = -20*min( float_profile$BBP700_smooth,
                                        na.rm = T) 
            float_profile=subset(float_profile,
                                 
                                 float_profile$BBP700_smooth>  QC_THRESHOLD_700  |
                                   float_profile$pressure<11 )
            
            
          }  # loop for  if ( min (  float_profile$BBP700_smoot
          
          
          # Remove the positive spike 
            float_profile$bbp_median_filter=rollmedian(float_profile$BBP700_smooth,
                                                       5,fill = "extend",na.pad = TRUE)
          
          # Moving smooth over depth 
            float_profile$bbp_final_qc=rollmean(float_profile$bbp_median_filter,
                                               5,fill = "extend",na.pad = TRUE)
          
          
  
          
        } #5
        
        profile_list[[i]]=  float_profile
      }#4
      
      
      # Step 3: final noise for each profile ---------------------------------------------
      
      float_data= bind_rows( profile_list)
      cycle_label=unique(float_data$cycle)
      
      
      # Step 5: save the processed bbp -------------------------------------------------------
      new_float_data=data.frame( label= paste(float_data$cycle,
                                              pressure=float_data$pressure)
      )
      new_float_data$bbp_qc= float_data$bbp_final_qc
      
      # Step 6: merge 到原来剖面  -------------------------------------------------------
      float_data_original$label=paste(float_data_original$cycle,
                                 pressure=float_data_original$pressure
      )
      
  #    float_data_raw=subset(float_data_raw,select=-c(bbp))
      float_data_original=left_join(   float_data_original,
                               new_float_data,by="label")
      
      
    }   # 3 # 从导入数据开始
    
    
    return(      float_data_original$bbp_qc)
    
    
} # end loop bbp_qc function





