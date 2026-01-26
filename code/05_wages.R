#Median wages by race, decile
# rolling12 mo

#import CPI monthly data, order by monthly date
cpi_data_mo <- realtalk::c_cpi_u_extended_monthly_sa |> 
  mutate(date = ymd(paste0(year,'-', month,'-1')))

# set base year to 2025
cpi2025 <- cpi_data_mo$c_cpi_u_extended[cpi_data_mo$date=="2025-08-01"]

wage_us_race <- org |>
  mutate(date = ymd(paste0(year,'-', month,'-1'))) |> 
  summarise(
      medwage = averaged_median(
        x = wage, 
        w = orgwgt,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(wbhao, date)) |> 
        # Join CPI-U data
 left_join(cpi_data_mo, by='date') |> 
  # inflation adjust wage data
 mutate(realwage = medwage*(cpi2025/c_cpi_u_extended)) |> 
  mutate(race = to_factor(wbhao)) |>
  arrange(race, date) |>  # ensure correct order within each group
  group_by(race) |>       # compute rolling mean per race
  mutate(realwage12 = rollmean(medwage, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  mutate(realwage12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, realwage12)) |> 
 select(date, race, realwage12, sample12) |> 
    # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = race,
    values_from = c(realwage12, sample12),
    names_glue = "{race}_{.value}"
  )

wage_dmv_race <- org_dmv |>
  mutate(date = ymd(paste0(year,'-', month,'-1'))) |> 
  summarise(
      medwage = averaged_median(
        x = wage, 
        w = orgwgt,  
        quantiles_n = 9L, 
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        .by=c(wbhao, date)) |> 
        # Join CPI-U data
  left_join(cpi_data_mo, by='date') |> 
  # inflation adjust wage data
  mutate(realwage = medwage*(cpi2025/c_cpi_u_extended)) |> 
  mutate(race = to_factor(wbhao)) |>
  arrange(race, date) |>  # ensure correct order within each group
  group_by(race) |>       # compute rolling mean per race
  mutate(realwage12 = rollmean(realwage, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
 mutate(realwage12 = if_else(is.na(sample12) | sample12 < 250,
                          NA_real_, realwage12)) |> 
 select(date, race, realwage12, sample12) |> 
    # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = race,
    values_from = c(realwage12, sample12),
    names_glue = "{race}_{.value}"
  )

test_dmv <- org_dmv |> 
  mutate(date = ymd(paste0(year,'-', month,'-1'))) |>
  summarize(n=n(),
.by=c(wbhao, date)) |> 
  mutate(wbhao=to_factor(wbhao))

p = c(10,20,30,40,50,60,70,80,90)

wage_us_deciles <- org |>
  mutate(date = ymd(paste0(year,'-', month,'-1'))) |> 
  reframe(
      decwage = averaged_quantile(
        x = wage,
        w = orgwgt,
        probs = p/100,
        na.rm = TRUE,
        quantiles_n = 9L,
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        decile = p,
        .by=date) |> 
    group_by(date) |> 
        # Join CPI-U data
  left_join(cpi_data_mo, by='date') |> 
  # inflation adjust wage data
  mutate(realwage = decwage*(cpi2025/c_cpi_u_extended)) |> 
  arrange(decile, date) |>  # ensure correct order within each group
  group_by(decile) |>       # compute rolling mean per race
  mutate(realwage12 = rollmean(realwage, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  mutate(realwage12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, realwage12)) |> 
 select(date, decile, realwage12, sample12) |> 
    # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = decile,
    values_from = c(realwage12, sample12),
    names_glue = "{decile}_{.value}"
  )

wage_dmv_deciles <- org_dmv |>
  mutate(date = ymd(paste0(year,'-', month,'-1'))) |> 
  reframe(
      decwage = averaged_quantile(
        x = wage,
        w = orgwgt,
        probs = p/100,
        na.rm = TRUE,
        quantiles_n = 9L,
        quantiles_w = c(1:4, 5, 4:1)),
        n=n(),
        decile = p,
        .by=date) |> 
    group_by(date) |> 
        # Join CPI-U data
  left_join(cpi_data_mo, by='date') |> 
  # inflation adjust wage data
  mutate(realwage = decwage*(cpi2025/c_cpi_u_extended)) |> 
  arrange(decile, date) |>  # ensure correct order within each group
  group_by(decile) |>       # compute rolling mean per race
  mutate(realwage12 = rollmean(realwage, k = 12, align = "right", fill = NA),
         sample12 = rollsum(n, k = 12, align = "right", fill = NA)) |> 
  ungroup() |> 
  mutate(realwage12 = if_else(is.na(sample12) | sample12 < 250,
                            NA_real_, realwage12)) |> 
 select(date, decile, realwage12, sample12) |> 
    # pivot wider — multiple value columns at once
  pivot_wider(
    names_from = decile,
    values_from = c(realwage12, sample12),
    names_glue = "{decile}_{.value}"
  )

wage_us <- left_join(wage_us_race, wage_us_deciles, by='date')
wage_dmv <- left_join(wage_dmv_race, wage_dmv_deciles, by='date')

#  Export epop tables to one workbook 
wb <- wb_workbook()

wb$ 
  # Add worksheet
  add_worksheet(sheet = "US wages")$
  add_data(x = wage_us)$
  add_worksheet(sheet = "DMV wages")$
  add_data(x = wage_dmv)

wb_save(wb, file = "output/wage_tables.xlsx", overwrite = TRUE)
