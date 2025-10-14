# Population statistics

# ##### Monthly population functions #####

# # Function to calculate rolling 12-month averages of population statistics ---
# pop_monthly_wide_with_metric <- function(df, groups, cohort_filter = TRUE) {

#   by_cols <- c("date", groups)

#   base <- df |>
#     filter(!!cohort_filter) |>
#     mutate(across(all_of(groups), ~ to_factor(.))) |>
#     summarize(
#       pop = sum(finalwgt, na.rm = TRUE),
#       n   = n(),
#       .by = all_of(by_cols)
#     ) |>
#     # compute rolling share = rolling label pop / rolling total pop that month
#     mutate(pop_share = pop / sum(pop), .by = date) |>
#     # unite first so we can roll per label series
#     unite("label", all_of(groups), sep = "-", remove = TRUE) |>
#     arrange(label, date) |>
#     mutate(
#       pop12 = slide_dbl(pop, mean, .before = 11, .complete = TRUE),
#       sample12   = slide_dbl(n,   sum, .before = 11, .complete = TRUE),
#       .by=label
#     )

#  # widen to one row per date, with columns per label/metric
#   base |>
#     select(date, label, pop_share, pop12, sample12) |>
#     pivot_wider(
#       id_cols     = date,
#       names_from  = label,
#       values_from = c(pop_share, pop12, sample12),
#       names_glue  = "{label}_{.value}"
#     ) |>
#     arrange(date)
# }


# Function to calculate rolling 12-month averages of population statistics ---
pop_monthly_wide_with_metric <- function(
  df,
  groups,
  cohort_filter = rlang::quo(TRUE),
  weight_var    = rlang::ensym(finalwgt),  # pass a symbol/quo, e.g. rlang::ensym(finalwgt)
  roll          = 12
) {
  by_cols <- c("date", groups)

 base <- df |>
    filter(!!cohort_filter) |>
    mutate(across(all_of(groups), ~ to_factor(.))) |>
    summarize(
      pop = sum(!!weight_var, na.rm = TRUE),
      sample   = n(),
      .by = all_of(by_cols)
    ) |>
  # single-month share by date
    mutate(emp_share = pop / sum(pop), .by = date) |>
  # roll per label
    unite("label", all_of(groups), sep = "-", remove = TRUE) |>
    arrange(label, date) |>
    mutate(
      # smooth population level (mean over 12 months)
      pop12_mean = slide_dbl(pop,    mean, .before = roll - 1, .complete = TRUE),
      # correct denominator for rolling shares (sum over 12 months)
      pop12_sum  = slide_dbl(pop,    sum,  .before = roll - 1, .complete = TRUE),
      # monthly sample size over 12 months
      sample12   = slide_dbl(sample, sum, .before = roll - 1, .complete = TRUE),
      .by        = label
    ) |>
    # 12m rolling share = rolling SUM by label / rolling SUM total that month
    mutate(pop_share12 = pop12_sum / sum(pop12_sum, na.rm = TRUE), .by = date)|>
    # mask shares where 12m avg sample is too small
    mutate(pop_share12 = if_else(is.na(sample12) | sample12 < 400,
                                 NA_real_, pop_share12))

  # wide: one row per date, columns per label/metric
  base |>
    transmute(
      date, label,
      pop_share, # single-month share
      pop_share12, # 12m rolling share (from sums)
      pop12 = pop12_mean, # 12m rolling MEAN population (smooth)
      sample12 # 12m rolling MEAN sample size
    ) |>
    pivot_wider(
      id_cols     = date,
      names_from  = label,
      values_from = c(pop_share12, pop12, sample12, pop_share),
      names_glue  = "{label}_{.value}"
    ) |>
    arrange(date)
}


##### Analysis ######

# Create dataframes based on geographies
basic_dmv <- basic |> filter(cbsafips == 47900)
basic_us  <- basic


us_cuts <- tibble(
  cut_name = c("race", "nativity", "age_bin", "educ25+", "has_children"),
  # Select grouping variables
  groups   = list("wbhao","native", "age_bin", "educ", "has_children"),
  # filters on dataframes
  cohort   = list(quo(TRUE), quo(TRUE), quo(TRUE), quo(age >= 25), quo(famrel == 1)),
  # Pick which dataset to analyze
  data     = list(basic_us, basic_us, basic_us, basic_us, basic_us),
  # Pick which weight to use in tabulations (in particular bc has_children should be tabulated using famwgt)      
  weight   = list(quo(finalwgt), quo(finalwgt), quo(finalwgt), quo(finalwgt), quo(finalwgt))
)

us_monthly <- pmap(
  us_cuts,
  \(cut_name, groups, cohort, data, weight, ...) {
    out <- pop_monthly_wide_with_metric(
      df            = data,
      groups        = groups,
      cohort_filter = cohort,
      weight_var    = weight
    )  }
) |> reduce(left_join, by = "date") |> 
  relocate(ends_with("pop_share12"), .after=date) |>
  relocate(ends_with("pop12"), .after = last_col()) |>
  relocate(ends_with("sample12"), .after = last_col()) |>
  relocate(ends_with("pop_share"), .after=last_col())


dmv_cuts <- tibble(
  cut_name = c("race", "nativity", "age_bin", "educ25+", "has_children"),
  # Select grouping variables
  groups   = list("wbhao","native", "age_bin", "educ", "has_children"),
  # filters on dataframes
  cohort   = list(quo(TRUE), quo(TRUE), quo(TRUE), quo(age >= 25), quo(famrel == 1)),
  # Pick which dataset to analyze
  data     = list(basic_dmv, basic_dmv, basic_dmv, basic_dmv, basic_dmv),
  # Pick which weight to use in tabulations (in particular bc has_children should be tabulated using famwgt)      
  weight   = list(quo(finalwgt), quo(finalwgt), quo(finalwgt), quo(finalwgt), quo(finalwgt))
)

dmv_monthly <- pmap(
  dmv_cuts,
  \(cut_name, groups, cohort, data, weight, ...) {
    out <- pop_monthly_wide_with_metric(
      df            = data,
      groups        = groups,
      cohort_filter = cohort,
      weight_var    = weight
    )

  }
) |> reduce(left_join, by = "date") |> 
  relocate(ends_with("pop_share12"), .after=date) |>
  relocate(ends_with("pop12"), .after = last_col()) |>
  relocate(ends_with("sample12"), .after = last_col()) |>
  relocate(ends_with("pop_share"), .after=last_col())

#  Export pop tables to one workbook 
wb <- wb_workbook()

wb$ 
  # Add worksheet
  add_worksheet(sheet = "US Monthly")$
  add_data(sheet = "US Monthly",  x = us_monthly)$
  add_worksheet(sheet = "DMV Monthly")$
  add_data(sheet = "DMV Monthly", x = dmv_monthly)

wb_save(wb, file = "output/population_tables.xlsx", overwrite = TRUE)
