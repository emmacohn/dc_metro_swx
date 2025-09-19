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

source('code/dmv_indicators.qmd')

# This script is incomplete and very messy. I hoped to create a similar function as in 
# 02_popstats.R to tabulate 12-month emp rates. Quarterly rates might suffice too
# source('code/03_lfstats.R')