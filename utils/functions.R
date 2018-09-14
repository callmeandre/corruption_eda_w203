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
