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
pacman::p_load(readr, ggplot2, dplyr, reshape, knitr, kableExtra, RColorBrewer,corrplot, grid, gridExtra)

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
#         @work_dir: directory folder location
#-----------------------------------------------------------------------------------

set_directories <- function(author=author){
  
  if(author=="Andre"){
    work_dir <- "C:/Users/afern/Desktop/Git/Berkeley/w203/corruption_eda_w203"
  }else if(author=="Keenan"){
    work_dir <- "Please add yourself to set_directories in functions.R in ~/utils/"
  }else if(author=="Erik"){
    work_dir <- "c:/other/mids/w203/homework/lab_1/corruption_eda_w203"
  }else{
    stop(paste0(author,": Please add yourself to set_directories in functions.R in ~/utils/"))
  }
  
  return(work_dir)
}

#-----------------------------------------------------------------------------------
# load_rda: 
#
#     description:
#         loads rda and assigns it to user-defined variable
#     parameters:
#         @loc: string
#     returns
#         @df: dataframe
#-----------------------------------------------------------------------------------

load_rda <- function(loc="data/Corrupt.Rdata"){
  
  df <- as.data.frame(eval(as.name(load(loc))))
  
  return(df)
}

#-----------------------------------------------------------------------------------
# multiplot: 
#
#     description:
#         returns multiple plots as one
#
#-----------------------------------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
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