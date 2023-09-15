
download_ancillary_dataset=function(){
  

   # Create the directory for storing the ancillary dataset --------------------------

  if (!dir.exists(paste(  root_path_tracer_budget_toolbox,
                           "Ancillary data and toolbox/Toolbox/",sep=""))){
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
            "Ancillary data and toolbox/Toolbox/",sep="")) 
  } # Bracket for "if (!dir.exists"

  if (!dir.exists(paste(  root_path_tracer_budget_toolbox,
                          "Ancillary data and toolbox/Ancilary data/NCEP_II/",sep=""))){
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/NCEP_II/",sep=""))
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/NCEP_II/Ice coverage",sep=""))
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/NCEP_II/relative humidity_NCEP_I",sep="")) 
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/NCEP_II/sea-level pressure",sep="")) 
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/NCEP_II/uwind",sep="")) 
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/NCEP_II/vwind",sep="")) 
  } # Bracket for "if (!dir.exists"
  
  
  if (!dir.exists(paste(  root_path_tracer_budget_toolbox,
                          "Ancillary data and toolbox/Ancilary data/WOA2018/",sep=""))){
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/WOA2018",sep=""))
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/WOA2018/no3",sep=""))
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/WOA2018/no3",sep=""))
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/WOA2018/salinity",sep="")) 
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/WOA2018/temperature",sep="")) 

  } # Bracket for "if (!dir.exists"
  
  
  if (!dir.exists(paste(  root_path_tracer_budget_toolbox,
                          "Ancillary data and toolbox/Ancilary data/Chla/",sep=""))){
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/Chla/",sep=""))
    
  } # Bracket for "if (!dir.exists"
  
  if (!dir.exists(paste(  root_path_tracer_budget_toolbox,
                          "Ancillary data and toolbox/Ancilary data/DIC/",sep=""))){
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/DIC/",sep=""))
    
  } # Bracket for "if (!dir.exists"
  
  if (!dir.exists(paste(  root_path_tracer_budget_toolbox,
                          "Ancillary data and toolbox/Ancilary data/TA/",sep=""))){
    
    dir.create(paste(  root_path_tracer_budget_toolbox,
                       "Ancillary data and toolbox/Ancilary data/TA/",sep=""))
    
  } # Bracket for "if (!dir.exists"
  
  
  ##Canyon-B toolbox ------------------------------------------------------
  
  if (!file.exists(paste(  root_path_tracer_budget_toolbox,
                           "Ancillary data and toolbox/Toolbox/Canyon-B toolbox",sep=""))){
    
    # download the data if Canyon-B toolbox does not exist
    print("Start donwnloading the Canyon-B toolbox from Github")
    
    # URL link in Github
    github_url <- "https://github.com/HCBScienceProducts/CANYON-B.git"
    
    # Define the directory for storing the Canyon-B toolbox
    local_dir <-paste(  root_path_tracer_budget_toolbox,
                        "Ancillary data and toolbox/Toolbox/Canyon-B toolbox",sep="")
    
    # Toolbox downloading 
    repo <- git2r::clone(github_url, local_dir)
    
    print("success in downloading Canyon-B toolbox from Github")
    
  }
  
  
  
  ## Check ONE-ARGO toolbox ------------------------------------------------------
  
  if (!file.exists(paste(  root_path_tracer_budget_toolbox,
                           "Ancillary data and toolbox/Toolbox/ONE-ARGO toolbox",sep=""))){
    
    # download the data if Canyon-B toolbox does not exist
    print("Start donwnloading the ONE-ARGO toolbox from Github")
    
    # URL link in Github
    github_url <- "https://github.com/NOAA-PMEL/OneArgo-R.git"
    
    # Define the directory for storing the Canyon-B toolbox
    local_dir <-paste(  root_path_tracer_budget_toolbox,
                        "Ancillary data and toolbox/Toolbox/ONE-ARGO toolbox",sep="")
    
    # Toolbox downloading 
    repo <- git2r::clone(github_url, local_dir)
    
    # Modify the file directory in the script 
    
    
    print("success in downloading ONE-ARGO toolbox from Github")
    
  }
  
  
  ## Check WOA nitrate product ------------------------------------------------------
  for (i in 1:12){
    
    # Define the directory for storing the NO3 product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/WOA2018/no3/woa18_all_n", 
                              sprintf("%02d", i),
                              "_01.nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA NO3 product does not exist
      print(paste("donwnload monthly WOA2018 no3 product: month",i))
      
      # URL
      url <- paste("https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/nitrate/all/1.00/woa18_all_n",
                   
                   sprintf("%02d", i),
                   "_01.nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  # Check annual NO3 product
  local_file_path <-paste(  root_path_tracer_budget_toolbox,
                            "Ancillary data and toolbox/Ancilary data/WOA2018/no3/woa18_all_n00_01.nc",
                            sep="")
  
  if (!file.exists(    local_file_path)){
    
    # download the data if WOA NO3 product  does not exist
    print("donwnloading annual WOA2018 no3 product")
    
    # URL
    url <- "https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/nitrate/all/1.00/woa18_all_n00_01.nc"
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    
  }
  
  
  
  ## Check WOA temperature product ------------------------------------------------------
  for (i in 1:12){
    
    # Define the directory for storing the NO3 product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/WOA2018/temperature/woa18_decav_t", 
                              sprintf("%02d", i),
                              "_01.nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload monthly WOA2018 temperature product: month",i))
      
      # URL
      url <- paste("https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/temperature/decav/1.00/woa18_decav_t",
                   
                   sprintf("%02d", i),
                   "_01.nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  # Check annual temperature product
  local_file_path <-paste(  root_path_tracer_budget_toolbox,
                            "Ancillary data and toolbox/Ancilary data/WOA2018/temperature/woa18_decav_t00_01.nc",
                            sep="")
  
  if (!file.exists(    local_file_path)){
    
    # download the data if WOA temperature product  does not exist
    print("donwnloading annual WOA2018 tempeature product")
    
    # URL
    url <- "https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/temperature/decav/1.00/woa18_decav_t00_01.nc"
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    
  }
  
  
  
  ## Check WOA salinity product ------------------------------------------------------
  for (i in 1:12){
    
    # Define the directory for storing the NO3 product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/WOA2018/salinity/woa18_decav_s", 
                              sprintf("%02d", i),
                              "_01.nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload monthly WOA2018 salinity product: month",i))
      
      # URL
      url <- paste("https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/salinity/decav/1.00/woa18_decav_s",
                   
                   sprintf("%02d", i),
                   "_01.nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  # Check annual salinity product
  local_file_path <-paste(  root_path_tracer_budget_toolbox,
                            "Ancillary data and toolbox/Ancilary data/WOA2018/salinity/woa18_decav_s00_01.nc",
                            sep="")
  
  if (!file.exists(    local_file_path)){
    
    # download the data if WOA tempeature product  does not exist
    print("donwnloading annual WOA2018 salinity product")
    
    # URL
    url <- "https://www.ncei.noaa.gov/thredds-ocean/fileServer/ncei/woa/salinity/decav/1.00/woa18_decav_s00_01.nc"
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    
  }
  
  
  ## Check NCEP U wind speed ------------------------------------------------------
  year_today<- year(Sys.time())
  for (i in 2012:year_today){
    
    # Define the directory for storing the NO3 product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/uwind/uwnd.10m.gauss.", 
                              i,
                              ".nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload NCEP 10m u-wind: year",i))
      
      # URL
      url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/gaussian_grid/uwnd.10m.gauss.",
                   i,".nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  ## Check NCEP V wind speed ------------------------------------------------------
  year_today<- year(Sys.time())
  for (i in 2012:year_today){
    
    # Define the directory for storing v wind product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/vwind/vwnd.10m.gauss.", 
                              i,
                              ".nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload NCEP 10m v-wind: year",i))
      
      # URL
      url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/gaussian_grid/vwnd.10m.gauss.",
                   i,".nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  
  
  ## Check NCEP ice coverage------------------------------------------------------
  year_today<- year(Sys.time())
  for (i in 2012:year_today){
    
    # Define the directory for storing the NCEP ice coverage 
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/Ice coverage/icec.sfc.gauss.", 
                              i,
                              ".nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload NCEP ice concentration: year",i))
      
      # URL
      url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/gaussian_grid/icec.sfc.gauss.",
                   i,".nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  
  ## Check NCEP sea level pressure------------------------------------------------------
  year_today<- year(Sys.time())
  for (i in 2012:year_today){
    
    # Define the directory for storing the NCEP sea-level pressure
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/sea-level pressure/mslp.", 
                              i,
                              ".nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload NCEP sea-level pressure: year",i))
      
      # URL
      url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/surface/mslp.",
                   i,".nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  
  
  
  ## Check relative humidity------------------------------------------------------
  year_today<- year(Sys.time())
  for (i in 2012:year_today){
    
    # Define the directory for storing the NCEP relative humidity
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/relative humidity_NCEP_I/rhum.sig995.", 
                              i,
                              ".nc",sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload NCEP relative humidity: year",i))
      
      # URL
      url <- paste("https://downloads.psl.noaa.gov//Datasets/ncep.reanalysis/Dailies/surface/rhum.sig995.",
                   i,".nc",sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path))"
  } # Bracket for "for (i in 1:12)"
  

  
  
  # Create the links for ocean color dataset
  start_date <- ymd("20120101")
  end_date <- as.Date(Sys.time())
  end_date <- ymd(end_date%m-% months(4)) # ocean color has some delay in updating the most recent data
  
  urls <- character()
  
  # generate the urls for monthly remotely sensed chl-a product (ocean)
  # according to the temporal range (from year 2010 to the most recent month)
  current_date <- start_date
  while (current_date <= end_date) {
   
    year_str <- format(current_date, "%Y")
    month_str <- format(current_date, "%m")
    day_str <- format(current_date, "%d")
    
    # Get the first day of the next month
    first_day_next_month <- floor_date(current_date %m+% months(1), "month")
    
    # Subtract one day to get the last day of the current month
    last_day <- first_day_next_month - 1
    
    # Extract year, month, and day as strings
    last_year_str <- format(last_day, "%Y")
    last_month_str <- format(last_day, "%m")
    last_day_str <- format(last_day, "%d")
    
    
    url <- paste("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/AQUA_MODIS.",
                 year_str, month_str, day_str, "_", last_year_str, last_month_str, last_day_str, ".L3m.MO.CHL.chlor_a.9km.nc",
                 sep = "")
    urls <- c(urls, url)
    
    current_date <- current_date %m+% months(1)
  }
  
  
  ## Check chla from Ocean color------------------------------------------------------
  i=2
  for (i in 1:length(urls)){
    
    file_name= stringr::str_sub (urls[i], -43,-1)  # Extract the file name
    # Define the directory for storing the NO3 product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/Chla/", 
                              file_name,
                              sep="")
    
    if (!file.exists(    local_file_path)){
      
      # download the data if WOA temperature product does not exist
      print(paste("donwnload surface chl-a from ocean color. Progress:",round(i/length( urls)*100,3),"%"))
      
      # URL
      url <- paste(urls[i],"?appkey=",appkey_Earthdata,sep="")
      
      curl_download(url,   local_file_path,
                    quiet=F) 
      
      
    } # Bracket for "if (!file.exists(    local_file_path)){"
  } # Bracket for "for (i in 1:length(links)){"
  
  
  
  ## Check DIC gridded product ------------------------------------------------------
  
  if (!file.exists(paste(  root_path_tracer_budget_toolbox,
                           "Ancillary data and toolbox/Ancilary data/DIC/TCO2_NNGv2LDEO_climatology.nc",sep=""))){
    
    # download the data if global DIC product does not exist
    print("Start donwnloading the global monthly DIC gridded product (~0.6Gb)")
    
    # URL
    url <- "https://digital.csic.es/bitstream/10261/200537/4/TCO2_NNGv2LDEO_climatology.nc"
    
    # Define the directory for storing the DIC gridded product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/DIC/TCO2_NNGv2LDEO_climatology.nc",sep="")
    
    # DIC product downloading 
    curl_download(url,   local_file_path,
                  quiet=F) 
    
    print("success in downloading global monthly DIC gridded product")
    
  }
  
  ## Check global TA gridded product ------------------------------------------------------
  
  if (!file.exists(paste(  root_path_tracer_budget_toolbox,
                           "Ancillary data and toolbox/Ancilary data/TA/AT_NNGv2_climatology.nc",sep=""))){
    
    # download the data if TA product  does not exist
    print("Start donwnloading the global monthly TA gridded product (~3.1 Gb)")
    
    # URL
    url <- "https://digital.csic.es/bitstream/10261/184460/6/AT_NNGv2_climatology.nc"
    
    # Define the directory for storing the TA product
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/TA/AT_NNGv2_climatology.nc",sep="")
    
    # TA product downloading 
    curl_download(url,   local_file_path,
                  quiet=F) 
    
    print("success in downloading global monthly TA gridded product")
    
  }
  
  
  # Archive the most recent NCEP dataset each time when running the toolbox-------------------------------------
  
  # Save the date for the last code operation, with the aim to limit the time of code 
  # execution(otherwise, the ID may be prohibited tempeorally if we request the files from NCEP multiple times each day)
  date_file <-paste(  root_path_tracer_budget_toolbox,
                      "Ancillary data and toolbox/Ancilary data/NCEP_II/last_execution_date.txt", 
                     sep="")
    
  # Obtain 
  current_date <- Sys.Date()
  
  # Generate a "date" file if it does not exist 
  # The "date" record file is to used to prevent from downloading NCEP file repeatly 
  # within a day. Otherwise, NCEP server may block our IP address 
  if (!file.exists(date_file)) {
    writeLines(as.character(current_date), date_file)
  }
  
  last_execution_date <- readLines( date_file )
  last_execution_date <- as.Date(last_execution_date)
  today <- Sys.Date()
  if (last_execution_date != today) {
    year_today<- year(Sys.time())
    
    # U wind
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/uwind/uwnd.10m.gauss.", 
                              year_today,
                              ".nc",sep="")
    
    url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/gaussian_grid/uwnd.10m.gauss.",
                 year_today,".nc",sep="")
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    print("Archive the most recent NCEP dataset: 10m u wind")
    
    # V wind
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/vwind/vwnd.10m.gauss.", 
                              year_today,
                              ".nc",sep="")
    
    url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/gaussian_grid/vwnd.10m.gauss.",
                 year_today,".nc",sep="")
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    print("Archive the most recent NCEP dataset: 10m v wind")
    
    # ice concentration
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/Ice coverage/icec.sfc.gauss.", 
                              year_today,
                              ".nc",sep="")
    
    url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/gaussian_grid/icec.sfc.gauss.",
                 year_today,".nc",sep="")
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    print("Archive the most recent NCEP dataset: ice concentration")
    
    # sea-level pressure
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/sea-level pressure/mslp.", 
                              year_today,
                              ".nc",sep="")
    
    url <- paste("https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis2/Dailies/surface/mslp.",
                 year_today,".nc",sep="")
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    print("Archive the most recent NCEP dataset: sea-level pressure")
    
    # relative humidity 
    local_file_path <-paste(  root_path_tracer_budget_toolbox,
                              "Ancillary data and toolbox/Ancilary data/NCEP_II/relative humidity_NCEP_I/rhum.sig995.", 
                              year_today,
                              ".nc",sep="")
    
    url <- paste("https://downloads.psl.noaa.gov//Datasets/ncep.reanalysis/Dailies/surface/rhum.sig995.",
                 year_today,".nc",sep="")
    
    curl_download(url,   local_file_path,
                  quiet=F) 
    print("Archive the most recent NCEP dataset: relative humidity")
    
    writeLines(as.character(today), date_file)
  } 
 
  
  
} # Bracket for "download_ancillary_dataset=function"


  
