
# derive the first derivative of the 
# function with respect to "x"

derive_1= function (x,y){ 
  
  
  
  y_length=length(y)
  p1_y=rep(0,  y_length)
  
  
  p1_y[2:  (y_length-1)  ]=(  y[3:  (y_length)  ] -  y[ 1 :  (y_length-2)  ] ) /    (x[3: (y_length)]-x[ 1 :  (y_length-2)]   )
  
  
  return(p1_y)
  
  
}