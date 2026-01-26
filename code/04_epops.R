# EPOPS by various cuts

# ##### Monthly functions #####

# Function to calculate rolling 12-month averages of statistics ---
epop_monthly_wide_with_metric <- function(
  df,
  groups,
  cohort_filter = rlang::quo(TRUE),
  weight_var    = rlang::ensym(finalwgt),  # pass a symbol/quo, e.g. rlang::ensym(orgwgt)
  roll          = 12
) {
  by_cols <- c("date", groups)

 base <- df |>
    filter(!!cohort_filter) |>
    mutate(across(all_of(groups), ~ to_factor(.))) |>
    
    summarize(
      epop = (emp*sum(!!weight_var, na.rm = TRUE))/sum(!!weight_var, na.rm = TRUE),
      sample = n(),
      .by = all_of(by_cols)
    ) |>
  # single-month share by date
   # mutate(epop_share = epop / sum(epop), .by = date) |>
  # roll per label
    unite("label", all_of(groups), sep = "-", remove = TRUE) |>
    arrange(label, date) |>
    mutate(
      # smooth LF level (mean over 12 months)
      epop12_mean = slide_dbl(epop,    mean, .before = roll - 1, .complete = TRUE),
      # correct denominator for rolling shares (sum over 12 months)
      #epop12_sum  = slide_dbl(epop,    sum,  .before = roll - 1, .complete = TRUE),
      # monthly sample size over 12 months
      sample12   = slide_dbl(sample, sum, .before = roll - 1, .complete = TRUE),
      .by        = label
    ) |>
    # 12m rolling share = rolling SUM by label / rolling SUM total that month
    #mutate(epop_share12 = epop12_sum / sum(epop12_sum, na.rm = TRUE), .by = date)|>
    # mask shares where 12m avg sample is too small
    mutate(epop12_mean = if_else(is.na(sample12) | sample12 < 300,
                                 NA_real_, epop12_mean))

  # wide: one row per date, columns per label/metric
  base |>
    transmute(
      date, label,
      #epop_share, # single-month share
      #epop_share12, # 12m rolling share (from sums)
      epop12 = epop12_mean, # 12m rolling MEAN epop (smooth)
      sample12 # 12m rolling MEAN sample size
    ) |>
    pivot_wider(
      id_cols     = date,
      names_from  = label,
      values_from = c(#epop_share12, 
        epop12, sample12), 
        #epop_share),
      names_glue  = "{label}_{.value}"
    ) |>
    arrange(date)
}


##### Analysis ######

# Create dataframes based on geographies
basic_dmv <- basic |> filter(cbsafips == 47900)
basic_us  <- basic


us_cuts <- tibble(
  cut_name = c("race", "age_bin", "all"),
  # Select grouping variables
  groups   = list("wbhao", "age_bin", "civilian_pop"),
  # filters on dataframes
  cohort   = list(quo(TRUE), quo(TRUE), quo(TRUE)),
  # Pick which dataset to analyze
  data     = list(basic_us, basic_us, basic_us),
  # Pick which weight to use in tabulations (in particular bc has_children should be tabulated using famwgt)      
  weight   = list(quo(finalwgt), quo(finalwgt), quo(finalwgt))
)

us_monthly <- pmap(
  us_cuts,
  \(cut_name, groups, cohort, data, weight, ...) {
    out <- epop_monthly_wide_with_metric(
      df            = data,
      groups        = groups,
      cohort_filter = cohort,
      weight_var    = weight
    )  }
) |> reduce(left_join, by = "date") |> 
 # relocate(ends_with("epop_share12"), .after=date) |>
  relocate(ends_with("epop12"), .after = date) |>
  relocate(ends_with("sample12"), .after = last_col())
 # relocate(ends_with("epop_share"), .after=last_col())


dmv_cuts <- tibble(
  cut_name = c("race", "age_bin", "all"),
  # Select grouping variables
  groups   = list("wbhao", "age_bin", "civilian_pop"),
  # filters on dataframes
  cohort   = list(quo(TRUE), quo(TRUE), quo(TRUE)),
  # Pick which dataset to analyze
  data     = list(basic_dmv, basic_dmv, basic_dmv),
  # Pick which weight to use in tabulations (in particular bc has_children should be tabulated using famwgt)      
  weight   = list(quo(finalwgt), quo(finalwgt), quo(finalwgt))
)

dmv_monthly <- pmap(
  dmv_cuts,
  \(cut_name, groups, cohort, data, weight, ...) {
    out <- epop_monthly_wide_with_metric(
      df            = data,
      groups        = groups,
      cohort_filter = cohort,
      weight_var    = weight
    )

  }
) |> reduce(left_join, by = "date") |> 
 # relocate(ends_with("epop_share12"), .after=date) |>
  relocate(ends_with("epop12"), .after = date) |>
  relocate(ends_with("sample12"), .after = last_col()) #|>
 # relocate(ends_with("epop_share"), .after=last_col())

#  Export lf tables to one workbook 
wb <- wb_workbook()

wb$ 
  # Add worksheet
  add_worksheet(sheet = "US Monthly")$
  add_data(sheet = "US Monthly",  x = us_monthly)$
  add_worksheet(sheet = "DMV Monthly")$
  add_data(sheet = "DMV Monthly", x = dmv_monthly)

wb_save(wb, file = "output/epop_tables.xlsx", overwrite = TRUE)
