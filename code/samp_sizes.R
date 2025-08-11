# This script analyzes sample size availability for the DC metro area
# https://microdata.epi.org/variables/geography/cbsafips/

library(tidyverse)
library(epiextractr)
library(labelled)

basic <- load_basic(2021:2025, year, month, basicwgt, orgwgt, minsamp, statefips, cbsafips,
                    age, female, wbhao, lfstat, unemp) |> 
  filter(cbsafips==47900) |> 
  filter(age>=16)

# Per, census, https://www2.census.gov/programs-surveys/cps/methodology/2015%20Geography%20Cover.pdf: 
#   47900 Washington-Arlington-Alexandria, DC-VA-MD-WV 

basic_samp <- basic |> 
  summarize(n=n(),
            wgt_n = sum(basicwgt, na.rm=TRUE),
            .by=c(year, month))

basic_urate <- basic |> 
  filter(lfstat %in% c(1,2)) |> 
  summarize(urate = weighted.mean(unemp, w=basicwgt, na.rm=TRUE),
            n=n(),
            .by=c(year, month))

org_samp <- basic |> 
  # keep ORG only
  filter(minsamp %in% c(4,8)) |> 
  summarize(n=n(),
            wgt_n = sum(orgwgt, na.rm=TRUE),
            .by=c(year, month))
