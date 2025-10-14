# Main file

library(tidyverse)
library(epiextractr)
library(epidatatools)
library(realtalk)
library(labelled)
library(janitor)
library(openxlsx)
library(openxlsx2)
library(here)
library(zoo)
library(slider)


source('code/01_cps_data.R')

source('code/02_popstats.R')

source('code/03_lfstats.R')

source('code/dmv_indicators.qmd')