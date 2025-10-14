# Labor force statistics

# ##### Monthly labor force functions #####

# Function to calculate rolling 12-month averages of LF statistics ---
lf_monthly_wide_with_metric <- function(
  df,
  groups,
  cohort_filter = rlang::quo(TRUE),
  weight_var    = rlang::ensym(orgwgt),  # pass a symbol/quo, e.g. rlang::ensym(orgwgt)
  roll          = 12
) {
  by_cols <- c("date", groups)

 base <- df |>
    filter(!!cohort_filter) |>
    mutate(across(all_of(groups), ~ to_factor(.))) |>
    summarize(
      emp = sum(!!weight_var, na.rm = TRUE),
      sample   = n(),
      .by = all_of(by_cols)
    ) |>
  # single-month share by date
    mutate(emp_share = emp / sum(emp), .by = date) |>
  # roll per label
    unite("label", all_of(groups), sep = "-", remove = TRUE) |>
    arrange(label, date) |>
    mutate(
      # smooth LF level (mean over 12 months)
      emp12_mean = slide_dbl(emp,    mean, .before = roll - 1, .complete = TRUE),
      # correct denominator for rolling shares (sum over 12 months)
      emp12_sum  = slide_dbl(emp,    sum,  .before = roll - 1, .complete = TRUE),
      # monthly sample size over 12 months
      sample12   = slide_dbl(sample, sum, .before = roll - 1, .complete = TRUE),
      .by        = label
    ) |>
    # 12m rolling share = rolling SUM by label / rolling SUM total that month
    mutate(emp_share12 = emp12_sum / sum(emp12_sum, na.rm = TRUE), .by = date)|>
    # mask shares where 12m avg sample is too small
    mutate(emp_share12 = if_else(is.na(sample12) | sample12 < 400,
                                 NA_real_, emp_share12))

  # wide: one row per date, columns per label/metric
  base |>
    transmute(
      date, label,
      emp_share, # single-month share
      emp_share12, # 12m rolling share (from sums)
      emp12 = emp12_mean, # 12m rolling MEAN emp (smooth)
      sample12 # 12m rolling MEAN sample size
    ) |>
    pivot_wider(
      id_cols     = date,
      names_from  = label,
      values_from = c(emp_share12, emp12, sample12, emp_share),
      names_glue  = "{label}_{.value}"
    ) |>
    arrange(date)
}


##### Analysis ######

# Create dataframes based on geographies
org_dmv <- org |> filter(cbsafips == 47900)
org_us  <- org


us_cuts <- tibble(
  cut_name = c("race", "nativity", "age_bin", "educ25+", "has_children"),
  # Select grouping variables
  groups   = list("wbhao","native", "age_bin", "educ", "has_children"),
  # filters on dataframes
  cohort   = list(quo(TRUE), quo(TRUE), quo(TRUE), quo(age >= 25), quo(famrel == 1)),
  # Pick which dataset to analyze
  data     = list(org_us, org_us, org_us, org_us, org_us),
  # Pick which weight to use in tabulations (in particular bc has_children should be tabulated using famwgt)      
  weight   = list(quo(orgwgt), quo(orgwgt), quo(orgwgt), quo(orgwgt), quo(orgwgt))
)

us_monthly <- pmap(
  us_cuts,
  \(cut_name, groups, cohort, data, weight, ...) {
    out <- lf_monthly_wide_with_metric(
      df            = data,
      groups        = groups,
      cohort_filter = cohort,
      weight_var    = weight
    )  }
) |> reduce(left_join, by = "date") |> 
  relocate(ends_with("emp_share12"), .after=date) |>
  relocate(ends_with("emp12"), .after = last_col()) |>
  relocate(ends_with("sample12"), .after = last_col()) |>
  relocate(ends_with("emp_share"), .after=last_col())


dmv_cuts <- tibble(
  cut_name = c("race", "nativity", "age_bin", "educ25+", "has_children"),
  # Select grouping variables
  groups   = list("wbhao","native", "age_bin", "educ", "has_children"),
  # filters on dataframes
  cohort   = list(quo(TRUE), quo(TRUE), quo(TRUE), quo(age >= 25), quo(famrel == 1)),
  # Pick which dataset to analyze
  data     = list(org_dmv, org_dmv, org_dmv, org_dmv, org_dmv),
  # Pick which weight to use in tabulations (in particular bc has_children should be tabulated using famwgt)      
  weight   = list(quo(orgwgt), quo(orgwgt), quo(orgwgt), quo(orgwgt), quo(orgwgt))
)

dmv_monthly <- pmap(
  dmv_cuts,
  \(cut_name, groups, cohort, data, weight, ...) {
    out <- lf_monthly_wide_with_metric(
      df            = data,
      groups        = groups,
      cohort_filter = cohort,
      weight_var    = weight
    )

  }
) |> reduce(left_join, by = "date") |> 
  relocate(ends_with("emp_share12"), .after=date) |>
  relocate(ends_with("emp12"), .after = last_col()) |>
  relocate(ends_with("sample12"), .after = last_col()) |>
  relocate(ends_with("emp_share"), .after=last_col())

#  Export lf tables to one workbook 
wb <- wb_workbook()

wb$ 
  # Add worksheet
  add_worksheet(sheet = "US Monthly")$
  add_data(sheet = "US Monthly",  x = us_monthly)$
  add_worksheet(sheet = "DMV Monthly")$
  add_data(sheet = "DMV Monthly", x = dmv_monthly)

wb_save(wb, file = "output/labor_force_tables.xlsx", overwrite = TRUE)
