# DESCRIPTION:
# This function is used to initialize the tracer budget toolbox

# PREREQUISITES: 
# - Set the R working directory to the root path of the tracer budget toolbox 

# Main procedures:
# 1. Load required R libraries 
# 2. Load a set of new functions designed for the tracer budget toolbox
# 3. Download and check the availability of ancillary paramters
# 4. Load the ONE-ARGO and Canyon-B toolbox 
# 5. Load the ERDDAP-R toolbox

# Creation DATE: JUNE 1, 2023 (Version 1.0)

# Load the required R packages from the Internet ------------------------------------------

# Check if the required packages are installed, if not, install them
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("lubridate")) install.packages("lubridate"); library(lubridate)
if (!require("plyr")) install.packages("plyr"); library(plyr)
if (!require("seacarb")) install.packages("seacarb"); library(seacarb)
if (!require("RNetCDF")) install.packages("RNetCDF"); library(RNetCDF)
if (!require("RNCEP")) install.packages("RNCEP"); library(RNCEP)
if (!require("zoo")) install.packages("zoo"); library(zoo) # contain the function "rollapply"
if (!require("openxlsx")) install.packages("openxlsx"); library("openxlsx") # open the "xlsx" file
if (!require("matrixStats")) install.packages("matrixStats"); library("matrixStats") # contain the function "rowMads"
if (!require("cmocean")) install.packages("cmocean"); library("cmocean") # contain the function "rowMads"
if (!require("gsw")) install.packages("gsw"); library("gsw") # load the functions for computing oxygen gas exchange 
if (!require("marelac")) install.packages("marelac"); library("marelac")# load the functions required in ERDDAP R toolbox
if (!require("parsedate")) install.packages("parsedate"); library("parsedate")# load the functions required in ERDDAP R toolbox
if (!require("plotdap")) install.packages("plotdap"); library("plotdap")# load the functions required in ERDDAP R toolbox
if (!require( "rerddap")) install.packages( "rerddap"); library( "rerddap")# load the functions required in ERDDAP R toolbox
if (!require( "rerddapXtracto")) install.packages("rerddapXtracto"); library("rerddapXtracto")# load the functions required in ERDDAP R toolbox
if (!require("graphics")) install.packages("graphics"); library("graphics")# load the functions required in ERDDAP R toolbox
if (!require( "mapdata")) install.packages( "mapdata"); library( "mapdata")# load the functions required in ERDDAP R toolbox
if (!require( "git2r")) install.packages( "git2r"); library( "git2r")# package used to download the toolbox from Github
if (!require( "curl")) install.packages( "curl"); library( "curl")# package used to download DIC and TA gridded product
if (!require( "mgcv")) install.packages( "mgcv"); library( "mgcv")# package used to download DIC and TA gridded product
if (!require( "nlme")) install.packages( "nlme"); library( "nlme") # package used to download DIC and TA gridded product


print ("Successfully loaded required libraries")



# Load a set of local scripts specifically created for the tracer-budget toolbox -----

# check if the user-defined pathway ends with "/"
# if not, we need to add the slash to ensure the secondary file directory work smoothly
if (substr(  root_path_tracer_budget_toolbox,
         nchar(  root_path_tracer_budget_toolbox),
         nchar(  root_path_tracer_budget_toolbox)) != "/" ){
  root_path_tracer_budget_toolbox=paste(root_path_tracer_budget_toolbox,
                                        "/",sep="")
}
# Load the main functions

  path_main_functions= paste(         root_path_tracer_budget_toolbox,
                                   "Main functions",
                                   sep="")
  setwd(  path_main_functions)
  
  script_names<- paste0(dir(  path_main_functions))
  # Exclude the initialization script
  script_names <- subset( script_names, script_names!="01_Initialize_tracer_budget_toolbox.R")
  # Source all main functions
  invisible(sapply( script_names,source,.GlobalEnv))
  
  ## Load the secondary functions ----------------------------------------------
  
  path_secondary_functions= paste(        root_path_tracer_budget_toolbox,
                                          "auxil",
                                          sep="")
  
  setwd(  path_secondary_functions)
  secondary_functions= dir(path_secondary_functions)
  invisible(sapply(paste0( secondary_functions),source,.GlobalEnv))
  print("Successfully loaded new functions designed for the tracer budget toolbox")
  
  
  # Check if the external toolbox and product are available  ----------------
  download_ancillary_dataset()
  # Load ONE-ARGO toolbox -------------------------------------------------

  path_code_ONE_Argo_toolbox= paste(   root_path_tracer_budget_toolbox,
                                     "Ancillary data and toolbox/Toolbox/ONE-ARGO toolbox",
                                     sep="")
  setwd(  path_code_ONE_Argo_toolbox)
  func.sources = list.files(  path_code_ONE_Argo_toolbox,pattern="*.R")
  func.sources = func.sources[which(func.sources %in% c('Tutorial.R',
                                                        "bgc_argo_workshop_R_license.R")==F)]
  
  if(length(grep("Rproj",func.sources))!=0){
    func.sources = func.sources[-grep("Rproj",func.sources)]
  }
  invisible(sapply(paste0(func.sources),source,.GlobalEnv))
  
  aux.func.sources = list.files(paste0(  path_code_ONE_Argo_toolbox,"/auxil"),pattern="*.R")
  invisible(sapply(paste0(  path_code_ONE_Argo_toolbox,"/auxil/",aux.func.sources),source,.GlobalEnv))
  initialize_argo() # Take some minutes to download the global Index
  
# Print the progress 
  print("Successfully initialized ONE-Argo-R toolbox")
  
# Load Canyon_B algorithm -------------------------------------------------
setwd(paste(    root_path_tracer_budget_toolbox ,
                             "Ancillary data and toolbox/Toolbox/Canyon-B toolbox" ,sep="" ))  
# Modify the file directory in the script of "CANYONB" to new path where Canyon-B is placed
line_to_modify <- 88
script_content <- readLines("CANYONB.R")
file_path_CANYONB=paste(    root_path_tracer_budget_toolbox ,
                            "Ancillary data and toolbox/Toolbox/Canyon-B toolbox/" ,sep="" )
new_line_content= paste( 'inputsdir<-"',file_path_CANYONB,'"',
                    sep="")

script_content[line_to_modify] <- new_line_content

writeLines(script_content, con =paste(    root_path_tracer_budget_toolbox ,
                                          "Ancillary data and toolbox/Toolbox/Canyon-B toolbox/CANYONB.R" ,sep="" ))


source ( "CANYONB.R" )

print("Successfully loaded the function of Canyon_B algorithm")

# Load the toolbox for accessing the data archived from ERDDAP ----------------------

  
  pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop(x, " :Package not found")
    }
  }
  
  # create list of required packages
  list.of.packages <- c("ncdf4", "parsedate", "rerddap","plotdap",
                        "rerddapXtracto", "graphics", "maps", "mapdata",
                        "ggplot2")
  
  # create list of installed packages
  pkges = installed.packages()[,"Package"]
  
  # Install and load all required pkgs
  for (pk in list.of.packages) {
    pkgTest(pk)
  }
  
  print("Successfully loaded the ERDDAP-R toolbox")