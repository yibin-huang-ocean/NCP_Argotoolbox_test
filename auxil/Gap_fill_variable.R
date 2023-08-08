

Gap_fill_variable = function(depth=float_profile_data$pressure,
              date=float_profile_data$date,
              variable=float_profile_data$tracer){
  
  variable_gap_filled=rep(NaN,length(        date))
  
  depth_label=unique(depth)
  i=1
  for (i in 1: length(  depth_label)){
    
    one_layer_label =which(depth==  depth_label[i])
    if (length(   one_layer_label)>3 &  Count_No_NA( variable[    one_layer_label])>1 ){# Only apply the gap-filling if the data number more than 3
      variable_gap_filled[    one_layer_label] = approx(   date[    one_layer_label],
                                                           variable[    one_layer_label],
                                                           date[    one_layer_label])$y
      
    } else{
      variable_gap_filled[    one_layer_label]= variable[    one_layer_label]
    }  # Bracket for " if (length(   one_layer_label)>3)"
   
    
  } # Bracket for "for (i in 1: length(  depth_label))"
  
  return(    variable_gap_filled)
}