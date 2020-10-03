Preparation of Maine DEP Annual CSO Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership

  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [2019 Report Data](#report-data)
      - [2008 Report Data](#report-data-1)
      - [Harmonize Names](#harmonize-names)
          - [Check Results](#check-results)
      - [Remove the Permit Numbers](#remove-the-permit-numbers)
  - [Save Data to File](#save-data-to-file)
  - [Identify Casco Bay CSO Communities by
    Name](#identify-casco-bay-cso-communities-by-name)
  - [Which Years](#which-years)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.2     v purrr   0.3.4
#> v tibble  3.0.3     v dplyr   1.0.2
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts ------------------------------------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)
```

# Load Data

## 2019 Report Data

``` r
dep_2019_Vol_data <- read_excel("2019_Report_Data.xlsx",
                                sheet = 'Volume 2019',
                                col_types = c('text', 'text',
                                              rep('numeric',17))) %>%
  rename_with(~ paste0('yr_', .), .cols = `1987`:`2019`) %>%
  mutate(Community = sub('\\*\\*\\*', '', Community)) %>%
  filter(! grepl('Total', Community))


dep_2019_Event_data <- read_excel("2019_Report_Data.xlsx",
                                  sheet = 'Events 2019',
                                col_types = c('text', 'text',
                                              rep('numeric',17))) %>%
  rename_with(~ paste0('yr_', .), .cols = `1987`:`2019`) %>%
  mutate(Community = sub('\\*\\*\\*', '', Community)) %>%
  filter(! grepl('Total', Community))


dep_2019_Outfall_data <- read_excel("2019_Report_Data.xlsx",
                                    sheet = 'Outfalls 2019',
                                col_types = c('text', 'text',
                                              rep('numeric',21))) %>%
  rename_with(~ paste0('yr_', .), .cols = `1987`:`2019`) %>%
  mutate(Community = sub('\\*\\*\\*', '', Community)) %>%
  filter(! grepl('Total', Community))
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in L19 / R19C12: got '-'
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in M19 / R19C13: got '-'
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in N19 / R19C14: got '-'
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in O19 / R19C15: got '-'
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in P19 / R19C16: got '-'
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in Q19 / R19C17: got '-'
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in R19 / R19C18: got '-'
```

## 2008 Report Data

``` r
dep_2008_Vol_data <- read_excel("2008_Report_Data.xlsx",
                                sheet = 'Volume 2008',
                                col_types = c('text', 'text',
                                              rep('numeric',22))) %>%
    rename_with(~ paste0('yr_', .), .cols = `1987`:`2008`) %>%
    filter(! grepl('Total', Community))
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in X19 / R19C24: got '-'


dep_2008_Event_data <- read_excel("2008_Report_Data.xlsx",
                                  sheet = 'Events 2008',
                                col_types = c('text', 'text',
                                              rep('numeric',22))) %>%
    rename_with(~ paste0('yr_', .), .cols = `1987`:`2008`)  %>%
    filter(! (grepl('Total', Community) |
                grepl('Mean', Community) |
                grepl('Median', Community)))
#> Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
#> Expecting numeric in X19 / R19C24: got '-'
```

## Harmonize Names

``` r
setdiff(dep_2019_Vol_data$Community, dep_2008_Vol_data$Community)
#> [1] "Cape Elizabeth"             "Hallowell W.D. - 2008 GAUD"
setdiff(dep_2008_Vol_data$Community, dep_2019_Vol_data$Community)
#> [1] "Cape Elizabeth (PWD)"     "Hallowell W.D.-2008 GAUD"

setdiff(dep_2019_Event_data$Community, dep_2008_Event_data$Community)
#> [1] "Cape Elizabeth" "Fort Kent U.D."
setdiff(dep_2008_Event_data$Community, dep_2019_Event_data$Community)
#> [1] "Cape Elizabeth (PWD)" "Fort KentU.D."

setdiff(dep_2019_Vol_data$Community, dep_2019_Event_data$Community)
#> [1] "Westbrook"
setdiff(dep_2019_Event_data$Community, dep_2019_Vol_data$Community)
#> [1] "Westbrook (PWD)"
```

``` r

dep_2019_Event_data$Community[dep_2019_Event_data$Community == 
                              "Westbrook (PWD)"] <- "Westbrook"
dep_2019_Outfall_data$Community[dep_2019_Outfall_data$Community == 
                              "Westbrook (PWD)"] <- "Westbrook"

dep_2008_Vol_data$Community[dep_2008_Vol_data$Community == 
                              "Cape Elizabeth (PWD)"] <- "Cape Elizabeth"
dep_2008_Vol_data$Community[dep_2008_Vol_data$Community == 
                              "Hallowell W.D.-2008 GAUD"] <- "Hallowell W.D. - 2008 GAUD"
dep_2008_Vol_data$Community[dep_2008_Vol_data$Community == 
                              "Cape Elizabeth (PWD)"] <- "Cape Elizabeth"
dep_2008_Vol_data$Community[dep_2008_Vol_data$Community == 
                             "Hallowell W.D.-2008 GAUD"] <- "Hallowell W.D. - 2008 GAUD"

dep_2008_Event_data$Community[dep_2008_Event_data$Community == 
                              "Cape Elizabeth (PWD)"] <- "Cape Elizabeth"
dep_2008_Event_data$Community[dep_2008_Event_data$Community == 
                              "Hallowell W.D.-2008 GAUD"] <- "Hallowell W.D. - 2008 GAUD"
dep_2008_Event_data$Community[dep_2008_Event_data$Community == 
                              "Cape Elizabeth (PWD)"] <- "Cape Elizabeth"
dep_2008_Event_data$Community[dep_2008_Event_data$Community == 
                             "Hallowell W.D.-2008 GAUD"] <- "Hallowell W.D. - 2008 GAUD"
```

### Check Results

``` r
setdiff(dep_2019_Vol_data$Community, dep_2008_Vol_data$Community)
#> character(0)
setdiff(dep_2008_Vol_data$Community, dep_2019_Vol_data$Community)
#> character(0)
```

## Remove the Permit Numbers

``` r
dep_2008_Vol_data     <- dep_2008_Vol_data     %>% select(-`NPDES Permit No.`)
dep_2008_Event_data   <- dep_2008_Event_data   %>% select(-`NPDES Permit No.`)
dep_2019_Vol_data     <- dep_2019_Vol_data     %>% select(-`NPDES Permit No.`)
dep_2019_Event_data   <- dep_2019_Event_data   %>% select(-`NPDES Permit No.`)
dep_2019_Outfall_data <- dep_2019_Outfall_data %>% select(-`NPDES Permit No.`)
```

``` r
vol_data <- dep_2008_Vol_data %>%
  select(Community, yr_1989:yr_2004) %>%
  full_join(dep_2019_Vol_data, by = 'Community') %>%
  relocate(c(yr_1987, yr_1988), .before = yr_1989) %>%
  pivot_longer(yr_1987:yr_2019, names_to = 'Year', values_to = 'Volume') %>%
  mutate(Year = as.numeric(substr(Year, 4, 7))) %>%
  filter(Year > 1996) # drop early data that is not accurate.

event_data <- dep_2008_Event_data %>%
  select(Community, yr_1989:yr_2004) %>%
  full_join(dep_2019_Event_data, by = 'Community') %>%
  relocate(c(yr_1987, yr_1988), .before = yr_1989) %>%
  pivot_longer(yr_1987:yr_2019, names_to = 'Year', values_to = 'Events') %>%
  mutate(Year = as.numeric(substr(Year, 4, 7)))

outfall_data <- dep_2019_Outfall_data %>%
  select (-`Year Unknown`) %>%
  pivot_longer(yr_1987:yr_2019, names_to = 'Year', values_to = 'Outfalls') %>%
  mutate(Year = as.numeric(substr(Year, 4, 7)))

the_data <- vol_data %>%
  full_join(event_data, by = c("Community", "Year")) %>%
  full_join(outfall_data, by = c("Community", "Year"))  %>%
  arrange(Community, Year)
  
```

``` r
rm(dep_2008_Event_data, dep_2008_Vol_data, dep_2019_Event_data,
   dep_2019_Outfall_data, dep_2019_Vol_data)
rm(vol_data, event_data, outfall_data)
```

# Save Data to File

``` r
write_csv(the_data, 'DEP_Annual_Totals.csv')
```

# Identify Casco Bay CSO Communities by Name

``` r
cbep_towns <- c("Cape Elizabeth",
                "Portland & PWD",
                "South Portland",
                "Westbrook",
                "Yarmouth")            
```

# Which Years

Many years included in the 2008 data are deeply suspect because the data
was estimated and not directly measured. Yarmouth ceased discharging by
CSO in 2001, and they never measured discharges, but estimated volumes
were always small.

``` r
first_measured <- tribble(~Town, ~Year,
"Cape Elizabeth", 2002,
"Portland & PWD", 1997,
"South Portland", 1997,
"Westbrook",      1997,
"Yarmouth",       NA)
knitr::kable(first_measured, caption = "Year CSO volumes were first regularly measured.")
```

| Town           | Year |
| :------------- | ---: |
| Cape Elizabeth | 2002 |
| Portland & PWD | 1997 |
| South Portland | 1997 |
| Westbrook      | 1997 |
| Yarmouth       |   NA |

Year CSO volumes were first regularly measured.

But the vast majority of volumes are from sites that have been measured
since 1997, so we track the data only since then. By doing so, I am
