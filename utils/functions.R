####################################################################################
# source functions for corruption_eda_w203.Rmd
# authors: Andre Fernandes, Keenan Szulik, and Erik Hou
#
#   this script is separated into two sections:
#       1. packages
#       2. functions
####################################################################################

####################################################################################
# Packages
####################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, ggplot2, dplyr, reshape, knitr, kableExtra)

####################################################################################
# Functions
####################################################################################

#-----------------------------------------------------------------------------------
# set_directories: 
#
#     description:
#         stores working directory information for each user and avoids merge conflicts
#     parameters:
#         @author: string
#     returns
#         directory folder location
#-----------------------------------------------------------------------------------

set_directories <- function(author=author){
  if(author=="Andre"){
    work_dir <- "C:/Users/afern/Desktop/Git/Berkeley/w203/corruption_eda_w203"
  }else if(author=="Keenan"){
    work_dir <- "Please add yourself to set_directories in functions.R in ~/utils/"
  }else if(author=="Erik"){
    work_dir <- "Please add yourself to set_directories in functions.R in ~/utils/"
  }else{
    stop(paste0(author,": Please add yourself to set_directories in functions.R in ~/utils/"))
  }
  return(work_dir)
}

# HERE IS A TEMPLATE FOR FUNCTIONS
#-----------------------------------------------------------------------------------
# function_name: 
#
#     description:
#         brief description about function
#     parameters:
#         @param1: object type
#         @param2: object type
#     returns
#         what does the function return if any
#-----------------------------------------------------------------------------------