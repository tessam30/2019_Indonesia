# Purpose: Set up repository for Indonesia Tableau Training
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_03_19
# Audience: USAID Indonesia Program Office 

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable", "sjmisc", "survey", "data.table", "lemon", "widyr", "RColorBrewer")
library(here)

# Create folders for project (if they do not exist)
dirs <- list("Data", "Dataout", "Tableau", "Images", "Scripts", "Logos")
map(dirs, ~dir.create(.))

datapath <- "Data"
admin1 <- "Data/BPS_2013Adm1_Boundary"
admin2 <- "Data/BPS_2013Adm 2_Boundary"
dataout <- "Dataout"
tableau <- "Tableau"
imagepath <- "Images"
rpath <- "Scripts"
logos <- "Logos"

# Migrate data to the folders, ignore as needed


# Fix time zone issues
Sys.setenv(TZ = "America/New_York")

# Source functions needed for scripts
files <- c("compare_vars.R", "strip_geom.R", "fltr_func.R", "group_check.R")
map(files, ~source(file.path(rpath, .)))