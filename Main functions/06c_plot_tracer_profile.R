plot_tracer_profile =function(float_profile_data,
                            tracer_budget  ){
  
  # DESCRIPTION:
  # This function returns a plot showing the time-series of tracer profile with mixed layer and integration depth embedded
  
  # INPUTS:
  #   float_profile_data (data.frame) : a data.frame file containing the processed float data, which is returned  
  #                                     from the function entitled "float_data_download_import_process"
  #   tracer_budget (data.frame)       : a data.frame file containing the tracer budget model output, which is returned  
  #                                     from the function entitled "Perform_tracer_budget" 

  
  # OUTPUTS:
  #  plot                             : a figure showing the time-series of tracer profile with mixed layer and integration depth embedded
  #  Creation DATE                    : JUNE 1, 2023  (Version 1.0)
  
  
  
  
  # Extract the deepest depth for the figure plot
  max_depth= max(c(tracer_budget$integration_depth_m))
  float_profile_data= subset(float_profile_data,
                             float_profile_data$pressure_m <   max_depth+50 )
  
  
  fig=ggplot()
  fig=fig+geom_hline(yintercept = 0)
  
  fig=fig+ geom_point(data=float_profile_data,
                     aes(  Convert_matlabtime( float_profile_data$date),
                         float_profile_data$pressure_m,
                         color=float_profile_data$tracer_umol_kg))

 
 # fig=fig+ geom_line(data=tracer_budget,
  #                   aes(tracer_budget$time,
   #                      tracer_budget$MLD_m),
    #                 size=1,linetype="dashed",color="blue")
  
  fig=fig+ geom_line(data=tracer_budget,
                     aes(tracer_budget$time,
                         tracer_budget$integration_depth_m),
                     size=1)
  
  
  fig=fig+ geom_line(data=tracer_budget,
                     aes(tracer_budget$time,
                         tracer_budget$integration_depth_m),
                     size=1)
  

  fig=fig+labs(fill = "")
  fig=fig+theme(legend.text = element_text(size = 12,face = 'bold',family = "serif"))# adjust the legend feature
  fig=fig+theme (axis.title.y = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) ##y轴名称加粗，加大
  fig=fig+theme (axis.title.x = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) ##x轴名称加粗，加大
  fig=fig+theme (axis.text.y = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) #y坐标轴加粗
  fig=fig+theme (axis.text.x = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) #x坐标轴加粗
  fig=fig+labs (family="serif",x="")  # modify the x-axis label
  fig=fig+labs (family="serif",y=expression (bold(Depth~"("~m~")")) )  # modify the y-axis label
  fig=fig+ scale_color_cmocean(name ='matter')
  fig <- fig + scale_y_reverse(expand=c(0,0)) 
  fig <- fig + scale_x_date(expand=c(0,0)) 
  fig <- fig+ labs(family="serif",
                   color = expression (bold(mu*mol~kg^-1)))
  
  fig
  
  return(fig)
  
}

