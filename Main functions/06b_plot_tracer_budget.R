plot_tracer_budget=function(tracer_budget,
                            show_cycle_dot=1,
                            Model_setting_list= Model_setting_list,
                            vertical_term_merge=1){
  # DESCRIPTION:
  # This function returns a plot showing the time-series of various tracer budget terms 

  # INPUTS:
  #   Model_setting_list (list)       : a list file containing the model setting, which is returned  
  #                                      from the function entitled "tracer_budget_toolbox_settings"
  #   tracer_budget (data.frame)      : a data.frame file containing the tracer budget model output, which is returned  
  #                                     from the function entitled "Perform_tracer_budget" 
  #   show_cycle_dot (numeric)        : 1. add the dots to indicate the float sampling point; 
  #                                     2. don't add the dots to indicate the float sampling point;  
  #   vertical_term_merge (numeric)   : 1. merge the three vertical terms
  #                                     2. don't merge the three vertical terms
  
  # OUTPUTS:
  #  plot                             : a figure showing the float trajectory 
  #  Creation DATE                    : JUNE 1, 2023  (Version 1.0)
  
 
  line_size=1
  dot_size=2
  fig=ggplot()
  fig=fig+geom_hline(yintercept = 0)
  
  if (Model_setting_list$EP_term_computation==1){
    
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$tracer_dt_mmol_m2_d1-tracer_budget$tracer_dt_error_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$tracer_dt_mmol_m2_d1+tracer_budget$tracer_dt_error_mmol_m2_d1,
                                  fill="Tracer change (salinity normalized)") ,alpha= 0.7,size=1)
    
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,tracer_budget$tracer_dt_mmol_m2_d1,
                           color="Tracer change (salinity normalized)"),size=  line_size)
    
    
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$tracer_background_dt_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$tracer_background_dt_mmol_m2_d1,
                                  fill="Background tracer change (salinity normalized)") ,alpha= 0.7,size=1)
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,
                           tracer_budget$tracer_background_dt_mmol_m2_d1,
                           color="Background tracer change (salinity normalized)"),size=  line_size)
    
    
  }
  
  if (Model_setting_list$EP_term_computation==2 |Model_setting_list$EP_term_computation==0){
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$EP_mmol_m2_d1-tracer_budget$EP_error_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$EP_mmol_m2_d1+tracer_budget$EP_error_mmol_m2_d1,
                                  fill="Evaporation and precipitation") ,alpha= 0.7,size=1)
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,
                           tracer_budget$EP_mmol_m2_d1,
                           color="Evaporation and precipitation"),size=  line_size)
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$tracer_dt_mmol_m2_d1-tracer_budget$tracer_dt_error_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$tracer_dt_mmol_m2_d1+tracer_budget$tracer_dt_error_mmol_m2_d1,
                                  fill="Tracer change") ,alpha= 0.7,size=1)
    
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,tracer_budget$tracer_dt_mmol_m2_d1,
                           color="Tracer change"),size=  line_size)
    
    if (Model_setting_list$background_correction!=1){
      fig = fig + geom_ribbon ( data=tracer_budget, 
                                aes(x=tracer_budget$time, 
                                    ymin=tracer_budget$tracer_background_dt_mmol_m2_d1,
                                    
                                    ymax=tracer_budget$tracer_background_dt_mmol_m2_d1,
                                    fill="Tracer (background) change") ,alpha= 0.7,size=1)
      
      fig=fig+ geom_line(data=tracer_budget,
                         aes(tracer_budget$time,
                             tracer_budget$tracer_background_dt_mmol_m2_d1,
                             color="Tracer (background) change"),size=  line_size)
    }

  }

  
  
 
  

  fig = fig + geom_ribbon ( data=tracer_budget, 
                            aes(x=tracer_budget$time, 
                                ymin=tracer_budget$gas_mmol_m2_d1-tracer_budget$gas_error_mmol_m2_d1,
                                
                                ymax=tracer_budget$gas_mmol_m2_d1+tracer_budget$gas_error_mmol_m2_d1,
                                fill="Gas exchange") ,alpha= 0.7,size=1)
  
  fig=fig+ geom_line(data=tracer_budget,
                     aes(tracer_budget$time,
                         tracer_budget$gas_mmol_m2_d1,
                         color="Gas exchange"),size=  line_size)
  
 
  
 

# Merge the physical transport terms --------------------------------------

  
  if (   vertical_term_merge==1){
    
   
    
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin= tracer_budget$vertical_diffusion_mmol_m2_d1+   
                                    tracer_budget$vertical_advection_mmol_m2_d1+
                                    tracer_budget$entrainment_mmol_m2_d1 -sqrt( tracer_budget$vertical_diffusion_error_mmol_m2_d1+   
                                           tracer_budget$vertical_advection_error_mmol_m2_d1^2+
                                           tracer_budget$entrainment_error_mmol_m2_d1^2),
                                  
                                  ymax= tracer_budget$vertical_diffusion_mmol_m2_d1+   
                                    tracer_budget$vertical_advection_mmol_m2_d1+
                                    tracer_budget$entrainment_mmol_m2_d1+sqrt( tracer_budget$vertical_diffusion_error_mmol_m2_d1+   
                                       tracer_budget$vertical_advection_error_mmol_m2_d1^2+
                                       tracer_budget$entrainment_error_mmol_m2_d1^2),
                              
                                   fill="Vertical transport") ,alpha= 0.7,size=1)
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,
                           tracer_budget$vertical_diffusion_mmol_m2_d1+   
                             tracer_budget$vertical_advection_mmol_m2_d1+
                             tracer_budget$entrainment_mmol_m2_d1,
                           color="Vertical transport"),size=  line_size)
    
    if (show_cycle_dot==1){
      
      fig=fig+ geom_point(data=tracer_budget,
                         aes(tracer_budget$time,
                             tracer_budget$vertical_diffusion_mmol_m2_d1+   
                               tracer_budget$vertical_advection_mmol_m2_d1+
                               tracer_budget$entrainment_mmol_m2_d1,
                             color="Vertical transport"),size=0.5)
      
    }
    
    
  } 
  
  if (   vertical_term_merge==2){
   
    
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$vertical_diffusion_mmol_m2_d1-tracer_budget$vertical_diffusion_error_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$vertical_diffusion_mmol_m2_d1+tracer_budget$vertical_diffusion_error_mmol_m2_d1,
                                  fill="Vertical diffusion") ,alpha= 0.7,size=1)
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,
                           tracer_budget$vertical_diffusion_mmol_m2_d1,
                           color="Vertical diffusion"),size=  line_size)
    
    
    
   
    
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$vertical_advection_mmol_m2_d1-tracer_budget$vertical_advection_error_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$vertical_advection_mmol_m2_d1+tracer_budget$vertical_advection_error_mmol_m2_d1,
                                  fill="Vertical advection") ,alpha= 0.7,size=1)
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,
                           tracer_budget$vertical_advection_mmol_m2_d1,
                           color="Vertical advection"))
 
    
    fig = fig + geom_ribbon ( data=tracer_budget, 
                              aes(x=tracer_budget$time, 
                                  ymin=tracer_budget$entrainment_mmol_m2_d1-tracer_budget$entrainment_error_mmol_m2_d1,
                                  
                                  ymax=tracer_budget$entrainment_mmol_m2_d1+tracer_budget$entrainment_error_mmol_m2_d1,
                                  fill="Entrainment") ,alpha= 0.7,size=1)
    
    
    
    fig=fig+ geom_line(data=tracer_budget,
                       aes(tracer_budget$time,
                           tracer_budget$entrainment_mmol_m2_d1,
                           color="Entrainment"),size=  line_size)
    
    if (   show_cycle_dot==1){
    
      
      fig=fig+ geom_point(data=tracer_budget,
                          aes(tracer_budget$time,
                              tracer_budget$vertical_diffusion_mmol_m2_d1,
                              color="Vertical diffusion"),size=0.5)
      fig=fig+ geom_point(data=tracer_budget,
                          aes(tracer_budget$time,
                              tracer_budget$vertical_advection_mmol_m2_d1,
                              color="Vertical advection"),size=0.5)
      
      fig=fig+ geom_point(data=tracer_budget,
                          aes(tracer_budget$time,
                              tracer_budget$entrainment_mmol_m2_d1,
                              color="Entrainment"),size=0.5)
     
     
      
      
      
    }
    
  }

 
  
  fig = fig + geom_ribbon ( data=tracer_budget, 
                            aes(x=tracer_budget$time, 
                                ymin=tracer_budget$Bio_mmol_m2_d1-tracer_budget$Bio_error_mmol_m2_d1,
                                
                                ymax=tracer_budget$Bio_mmol_m2_d1+tracer_budget$Bio_error_mmol_m2_d1,
                                fill="Biology") ,alpha= 0.7,size=1)
  
  fig=fig+ geom_line(data=tracer_budget,
                     aes(tracer_budget$time,
                         tracer_budget$Bio_mmol_m2_d1,
                         color="Biology"),size=  line_size)
  
  
  if (   show_cycle_dot==1){
    fig=fig+ geom_point(data=tracer_budget,
                        aes(tracer_budget$time,
                            tracer_budget$tracer_dt_mmol_m2_d1,
                            color="Tracer change"),size=0.5)
    fig=fig+ geom_point(data=tracer_budget,
                        aes(tracer_budget$time,
                            tracer_budget$gas_mmol_m2_d1,
                            color="Gas exchange"),size=0.5)
    fig=fig+ geom_point(data=tracer_budget,
                        aes(tracer_budget$time,
                            tracer_budget$EP_mmol_m2_d1,
                            color="Evaporation and preciption"),size=0.5)
    
    
    fig=fig+ geom_point(data=tracer_budget,
                        aes(tracer_budget$time,
                            tracer_budget$tracer_background_dt_mmol_m2_d1,
                            color="Tracer (background) change"),size=0.5)
    
    
    
    fig=fig+ geom_point(data=tracer_budget,
                        aes(tracer_budget$time,
                            tracer_budget$Bio_mmol_m2_d1,
                            color="Biology"),size=0.5)
  }
  
  
  fig=fig+guides(fill = guide_legend(), color = "none")
  fig=fig+labs(fill = "")
 fig=fig+theme(legend.text = element_text(size = 12,face = 'bold',family = "serif"))# adjust the legend feature
  fig=fig+theme (axis.title.y = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) ##y轴名称加粗，加大
  fig=fig+theme (axis.title.x = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) ##x轴名称加粗，加大
  fig=fig+theme (axis.text.y = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) #y坐标轴加粗
  fig=fig+theme (axis.text.x = element_text(size=12,colour = "black",face = "bold",family = "Times New Roman") ) #x坐标轴加粗
  fig=fig+labs (family="serif",x="")  
  fig=fig+labs (family="serif",y=expression (bold(mmol~m^-2~day^-1)) ) 
  fig=fig+scale_fill_brewer(palette = "Set1")+
    scale_color_brewer(palette = "Set1")
 
  
  
  
  return(fig)
  
}

