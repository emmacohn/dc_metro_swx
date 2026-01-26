basic_dmv <- basic |> filter(cbsafips == 47900, age >= 16)
basic_us  <- basic |> filter(age >= 16)

#EPOPs by race, age_bin, all

epop_dmv_all <- basic_dmv |>
  summarise(total_pop = sum(civilian_pop * finalwgt/12, na.rm=TRUE),
            total_emp = sum(emp * finalwgt/12, na.rm=TRUE),
            n=n(),
            .by=c(date)) |>
  mutate(epop = total_emp/total_pop) |> 
  arrange(date) |> 
  mutate(all_epop12 = rollmean(epop, k = 12, align = "right", fill = NA),
         all_sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  select(date, all_epop12, all_sample12) |>
  mutate(all_epop12 = if_else(is.na(all_sample12) | all_sample12 < 250,
                            NA_real_, all_epop12))

epop_dmv_race <- basic_dmv |>
  summarise(total_pop = sum(civilian_pop * finalwgt/12, na.rm=TRUE),
            total_emp = sum(emp * finalwgt/12, na.rm=TRUE),
            n=n(),
            .by=c(wbhao, date)) |>
  mutate(epop = total_emp/total_pop,
         race = to_factor(wbhao)) |> 
  arrange(race, date) |>  # ensure correct order within each group
  group_by(race) |>       # compute rolling mean per race
  mutate(epop12 = rollmean(epop, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  select(date, race, epop12, sample12) |>
  mutate(epop12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, epop12)) |> 
  # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = race,
    values_from = c(epop12, sample12),
    names_glue = "{race}_{.value}"
  )

epop_dmv_age <- basic_dmv |>
  summarise(total_pop = sum(civilian_pop * finalwgt/12, na.rm=TRUE),
            total_emp = sum(emp * finalwgt/12, na.rm=TRUE),
            n=n(),
            .by=c(age_bin, date)) |>
  mutate(epop = total_emp/total_pop,
         age = to_factor(age_bin)) |> 
  arrange(age, date) |>  # ensure correct order within each group
  group_by(age) |>       # compute rolling mean per race
  mutate(epop12 = rollmean(epop, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  select(date, age, epop12, sample12) |>
  mutate(epop12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, epop12)) |> 
  # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = age,
    values_from = c(epop12, sample12),
    names_glue = "{age}_{.value}"
  )

epop_dmv <- left_join(epop_dmv_all, epop_dmv_race, by='date') |> 
  left_join(epop_dmv_age)

epop_us_all <- basic_us |>
  summarise(total_pop = sum(civilian_pop * finalwgt/12, na.rm=TRUE),
            total_emp = sum(emp * finalwgt/12, na.rm=TRUE),
            n=n(),
            .by=c(date)) |>
  mutate(epop = total_emp/total_pop) |> 
  arrange(date) |> 
  mutate(all_epop12 = rollmean(epop, k = 12, align = "right", fill = NA),
         all_sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  select(date, all_epop12, all_sample12) |>
  mutate(all_epop12 = if_else(is.na(all_sample12) | all_sample12 < 250,
                            NA_real_, all_epop12))

epop_us_race <- basic_us |>
  summarise(total_pop = sum(civilian_pop * finalwgt/12, na.rm=TRUE),
            total_emp = sum(emp * finalwgt/12, na.rm=TRUE),
            n=n(),
            .by=c(wbhao, date)) |>
  mutate(epop = total_emp/total_pop,
         race = to_factor(wbhao)) |> 
  arrange(race, date) |>  # ensure correct order within each group
  group_by(race) |>       # compute rolling mean per race
  mutate(epop12 = rollmean(epop, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  select(date, race, epop12, sample12) |>
  mutate(epop12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, epop12)) |> 
  # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = race,
    values_from = c(epop12, sample12),
    names_glue = "{race}_{.value}"
  )

epop_us_age <- basic_us |>
  summarise(total_pop = sum(civilian_pop * finalwgt/12, na.rm=TRUE),
            total_emp = sum(emp * finalwgt/12, na.rm=TRUE),
            n=n(),
            .by=c(age_bin, date)) |>
  mutate(epop = total_emp/total_pop,
         age = to_factor(age_bin)) |> 
  arrange(age, date) |>  # ensure correct order within each group
  group_by(age) |>       # compute rolling mean per race
  mutate(epop12 = rollmean(epop, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  select(date, age, epop12, sample12) |>
  mutate(epop12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, epop12)) |> 
  # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = age,
    values_from = c(epop12, sample12),
    names_glue = "{age}_{.value}"
  )

epop_us <- left_join(epop_us_all, epop_us_race, by='date') |> 
  left_join(epop_us_age)

#  Export epop tables to one workbook 
wb <- wb_workbook()

wb$ 
  # Add worksheet
  add_worksheet(sheet = "US EPOPs")$
  add_data(x = epop_us)$
  add_worksheet(sheet = "DMV EPOPs")$
  add_data(x = epop_dmv)

wb_save(wb, file = "output/epop_tables.xlsx", overwrite = TRUE)