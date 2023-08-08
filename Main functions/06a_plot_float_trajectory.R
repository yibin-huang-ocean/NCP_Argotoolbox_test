
plot_float_trajectory=function(tracer_budget,
                               Model_setting_list =  Model_setting_list){
  
  # DESCRIPTION:
  # This function returns a plot showing the float trajectory 
  # The plot would be slightly different if the user selects the "Eularian correction" for background correction
  
  # INPUTS:
  #   Model_setting_list (list) : a list file containing the model setting, which is returned  
  #                                     from the function entitled "tracer_budget_toolbox_settings"
  #   tracer_budget (data.frame)       : a data.frame file containing the tracer budget model output, which is returned  
  #                                     from the function entitled "Perform_tracer_budget" 
  
  
  # OUTPUTS:
  #  plot                             : a figure showing the float trajectory 
  #  Creation DATE                    : JUNE 1, 2023  (Version 1.0)
  
  
  
  # get the ocean map
  world = map_data("world")
  

  g1 = ggplot()

  if (  Model_setting_list$background_correction!=3){
    g1=g1+geom_point( data=tracer_budget, aes(x=tracer_budget$longitude_E, 
                                              y=tracer_budget$latitude_N,
                                              color=tracer_budget$cycle),
                      size=1)
    
    g1= g1+cmocean::scale_color_cmocean(name ='deep')
    g1
    
  }
  
  if (  Model_setting_list$background_correction==3){
    
    g1=g1+geom_point( data=tracer_budget, aes(x=tracer_budget$longitude_E, 
                                              y=tracer_budget$latitude_N,
                                              color="All"
                                             ))
    
    
    tracer_budget_PS= subset(tracer_budget,tracer_budget$season_label=="P")
    
    g1=g1+geom_point( data=    tracer_budget_PS, aes(x=    tracer_budget_PS$longitude_E, 
                                                     y=    tracer_budget_PS$latitude_N,
                                                     color="Productive season (PS)"))
    
    g1=g1+geom_point( data=tracer_budget, aes(x=tracer_budget$longitude_median_PS_E, 
                                            y=tracer_budget$latitude_median_PS_N,color="Median location during each PS"),
                      size=5)
    
   
  }
 
  
  
  g1= g1+ theme_bw() +
    geom_polygon(data=world, aes(x=long, y=lat, group=group), 
                 fill="#dddddd")
  g1 = g1 +
    coord_cartesian(xlim=c( min(tracer_budget$longitude_E),  max(tracer_budget$longitude_E)),
                    ylim=c( min(tracer_budget$latitude_N),  max(tracer_budget$latitude_N))
                    
                    ) +
    theme(axis.title.x = element_text(size=18, colour="black", face="bold", family="serif"), 
          axis.text.x = element_text(size=18, colour="black", face="bold", family="serif"),
          axis.title.y = element_text(size=18, colour="black", face="bold", family="serif"), 
          axis.text.y = element_text(size=18, colour="black", face="bold", family="serif") ) +
    labs(x =expression (bold(Longitude~"("~"°"~E~")")),
         y =expression (bold(Latitude~"("~"°"~N~")")) ) +
    labs(color = "Cycle")+
    theme(legend.text = element_text(size = 10,face = 'bold',family = "serif"))+
    theme(legend.title = element_text(size = 10,face = 'bold',family = "serif"))+
    theme(plot.title = element_text(size = 0, face = "bold",family = "serif"))
 
  return(g1)
  
  
}