
#Load CPI data for inflation adjusting from realtalk
cpi <- cpi_u_annual
cpi2024 <- cpi$cpi_u[cpi$year==2024]

# Create basic dataframe
basic <- load_basic(2019:2025, 
                    year, month, basicwgt, orgwgt, finalwgt,
                    minsamp, statefips, cbsafips, wage, hoursu1i,
                    age, female, wbhao, educ, selfinc,
                    selfemp, lfstat, unemp, emp, nilf,
                    cow1, pubfed, citistat, pubsec,
                    # family and household vars
                    hhid, famid, famtype, famrel, famwgt) |> s
  # Create date in yyyy-mm-dd format
  mutate(date = ymd(paste0(year,'-', month,'-1'))) |> 
  # Create foreign-born variable
  mutate(native = case_when(citistat %in% c(1:3) ~ 1,
                              citistat %in% c(4,5) ~ 0)) |> 
  # Age bin variable
  mutate(age_bin = case_when(
                             age %in% c(0:15) ~ 0,
                             age %in% c(16:24) ~ 1, 
                             age %in% c(25:54) ~ 2,
                             age >=55 ~ 3)) |> 
  # Family information
 # create inclusive family id
    #note: inclusive ~ breaking out non-trad living arrangements as separate families
    # 1. separate non-family household members into individual families
    mutate(famnum = case_when(
      # non-family members receive new unique family id
      famid == 0 ~ row_number()*10,
      # family members & related subfamily assigned as related family
      famid > 1 & famtype == 3 ~ 1,
      # all other members assigned as subfamily members
      famid > 0 ~ famid)) |> 
    # 2. create family size for inclusive family units 
        # (non-family household members ~ individual families)
    # mutate(famsize = n(), .by = c(hhid, year, month, famnum)) |> 
    # 4. indicator for non-traditional (mixed) living arrangements
    # 1. identify number of children per family
    mutate(child = if_else(famrel == 3 & age < 18, 1, 0)) |> 
    mutate(childsize = sum(child, na.rm = TRUE), .by = c(hhid, year, month, famnum)) |>
  # count if family has at least one child under 18 living with them
  mutate(has_children = ifelse(childsize>0, yes=1, no=0))|> 
  add_value_labels(native = c('Native' = 1, 'Foreign-born'=0),
                  age_bin = c('0-15' = 0, '16–24' = 1, '25–54'=2, '55+'=3),
                  has_children = c('Has children' = 1, 'No children' = 0)) |> 
       
    # create quarters
    mutate(quarter = case_match(
      month,
      1:3 ~ 1,
      4:6 ~ 2,
      7:9 ~ 3,
      10:12 ~ 4
    )) |> 
    # create labor force indicators
    mutate(unemployed = case_when(
      lfstat == 1 ~ 0L,
      lfstat == 2 ~ 1L,
      .default = NA
    )) |> 
    # labor force participation
    mutate(lfp = if_else(lfstat == 1 | lfstat == 2, 1, 0)) |> 
    # part-time
    mutate(part_time = case_when(
      year < 1994 ~ NA,
      emp == 1 & hoursu1 <  35 ~ 1,
      emp == 1 & hoursu1 >= 35 ~ 0,
      emp == 1 & is.na(hoursu1) ~ 0
    )) |> 
    mutate(civilian_pop = 1) |> 
    # identify working age population (16–64)
    mutate(working_age_pop = if_else(age >= 16 & age <= 64, 1, 0))
           

# Create org dataframe
org <- basic |>
  #Keep ORG months only
  filter(minsamp %in% c(4,8)) |> 
  # Workers 16+, employed, not self-employed or self-incorporated.
  filter(age >= 16, orgwgt > 0, selfemp == 0) |> 
  # Join CPI-U data
  left_join(cpi, by='year') |> 
  # inflation adjust wage data
  mutate(realwage = wage*(cpi2024/cpi_u))

