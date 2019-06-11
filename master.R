# Pre-requisites -----------------------------------------------------------
require(lubridate)
require(hillr)
require(tidyverse)

## Human decisions 

quality_code_acceptable_minimum <- 0 #set to lowest level of acceptable data

yearReport <- 2016

endpoint <- "http://hydro.marlborough.govt.nz/soewq.hts?"

makeGraphs <- TRUE #run build visualisations code block
saveGraphs <- FALSE #save graphs in figs for reports

exportWrangle <- TRUE #push wrangled data as .RDS to location for shinymap / storymap
loadFromSaved <- TRUE #collect visualisation data from last wrangle push

##### Do not edit below

source("R/funcs_SWQ.R")

datestr <- stamp("24/02/1985")
yearstr <- stamp_date("2009")
fulldatestr <- stamp("24/02/1985 06:48:59")
filestampstr <- stamp("19850224")

param_order <- c("Turbidity", "SINitrogen", "Phosphorus", "Ecoli", 
                 "pH", "Oxygen", "Temperature", "Ammonia", "Nitrate")


# Load & Wrangle ----------------------------------------------------------

source("loadAndWrangle.R")


# Visualise ---------------------------------------------------------------

source("buildGraphs.R")
