

# Labor force statistics

# Analyzes the labor force composition of CBSAFIPS 47900 Washington-Arlington-Alexandria, DC-VA-MD-WV
# <https://www2.census.gov/programs-surveys/cps/methodology/2015%20Geography%20Cover.pdf:>

# labforce_data <- basic |> 
#     filter(age>=16) 

dmv_laborforce <- basic |> 
  filter(age>=16, cbsafips==47900, nilf==0) |> 
  mutate(across(nilf|wbhao, ~to_factor(.x))) |> 
  summarize(
    labforce = sum(basicwgt, na.rm = TRUE),
    sample_size = n(),
    .by = c(date, wbhao)
  ) |> 
  mutate(pct = labforce / sum(labforce),
.by = c(date)) 
# |> 
#     mutate(
#       laborforce12 = slide_dbl(labforce, sum, .before = 11, .complete = TRUE),
#       n12   = slide_dbl(sample_size,   sum, .before = 11, .complete = TRUE),
#       .by=date
#     ) |>
#     # compute rolling share = rolling label pop / rolling total pop that month
#     mutate(pop_share12 = laborforce12 / sum(laborforce12, na.rm = TRUE),
#     .by=date)

#   arrange(date, wbhao) |>
  


# |> 
#   mutate(percent = count / universe) |> 
#   arrange(date) |> 
#   mutate(
#     count12     = slide_mean(percent, before = 11, complete = TRUE),
#     percent12     = slide_mean(percent, before = 11, complete = TRUE),
#     sample_size12 = slide_sum(sample_size, before = 11, complete = TRUE)) |> 
#     # Convert by_var selections to labels
#   mutate(nilf = to_factor(nilf))


# lf_vars <- list('emp', 'nilf')
# by_groups <- list("wbhao", "native", 'age_bin', 'educ')

##### Analysis

# DMV overall
dmv_all <- basic |> 
  filter() |> 
  calc_emp_indicators()
  basic,
  lfstat %in% c(1,2) & cbsafips == 47900,
  geo = "DMV"
) 


# Prime-age DMV by age_bin
dmv_prime_by_agebin <- calc_emp_indicators(
  basic,
  lfstat %in% c(1,2) & cbsafips == 47900 & age_bin %in% 1:3,
  groups = "age_bin",
  geo = "DMV"
) |> 
  mutate(age_bin = to_factor(age_bin))


# DC overall
dc_all <- calc_emp_indicators(
  basic,
  lfstat %in% c(1,2) & statefips == 11,
  geo = "DC"
)

# US overall
us_all <- calc_emp_indicators(
  basic,
  lfstat %in% c(1,2),
  geo = "US"
)
    