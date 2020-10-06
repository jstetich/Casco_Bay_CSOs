Preparation of Portland CSO Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership

  - [Load Libraries](#load-libraries)
  - [Establish Folder Reference](#establish-folder-reference)
  - [Read Rain Data](#read-rain-data)
      - [Find the Rain Files.](#find-the-rain-files.)
      - [Read Each Rain File](#read-each-rain-file)
  - [Write Rainfall Data File](#write-rainfall-data-file)
      - [Combine Data](#combine-data)
  - [Load CSO Event Files](#load-cso-event-files)
      - [Collect File Names](#collect-file-names)
          - [Code to Extract Year From Each
            Filename](#code-to-extract-year-from-each-filename)
      - [Read in 2015 Data](#read-in-2015-data)
          - [List Locations](#list-locations)
          - [Read 2015 Data](#read-2015-data)
          - [Error Correction](#error-correction)
          - [Reprocess to Match Other File
            Format](#reprocess-to-match-other-file-format)
          - [Which CSOs Were Monitored All Year in
            2015?](#which-csos-were-monitored-all-year-in-2015)
      - [Read the Other Files](#read-the-other-files)
          - [2016 Data](#data)
          - [2017 Data](#data-1)
          - [2018 Data](#data-2)
          - [2019 Data](#data-3)
  - [Save Five Years of Portland CSO Event
    Data](#save-five-years-of-portland-cso-event-data)
      - [Combine Data](#combine-data-1)
  - [Export Data for GIS](#export-data-for-gis)
      - [Calculate Totals by CSO](#calculate-totals-by-cso)
      - [Export File](#export-file)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

# Establish Folder Reference

``` r
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
niecenm  <- 'Portland_Data'
niece    <- file.path(sibling, niecenm)
```

# Read Rain Data

Notice that the DATES in these excel files are incorrect. They are
displayed in the spreadsheets as dates, showing only Month and Day, but
the underlying data is not necessarily from the year the data
represents. To correct for that, I assemble the date from day, month,
and

## Find the Rain Files.

``` r
fns <- list.files(niece, pattern = 'Rain')
print(fns)
```

    ## [1] "2015_Portland_Rain_Data.xlsx" "2016_Portland_Rain_Data.xlsx"
    ## [3] "2017_Portland_Rain_Data.xlsx" "2018_Portland_Rain_Data.xlsx"
    ## [5] "2019 Portland Rain Data.xlsx"

## Read Each Rain File

We use a map\_df() call to iterate across all the (monthly) dates and
assemble single annual rainfall record.

``` r
for (fn in fns) {
  # The first four characters of each file name gve the year
  year = as.numeric(substr(fn,1,4))
  fpath <- file.path(niece,fn)
  
  # create a unique r symbol (name) for the each year's rain data
  dataname = paste('raindata',as.character(year), sep = '_')
  
  # Use map to iterate across all pages
  pages <- excel_sheets(fpath)
  test <- map_df(pages, function(p) read_excel(fpath, sheet = p,
                                               range = "A3:L34", skip = 2,
                                   col_names = c("draftdate", "AADaily", "AAMax",
                                                 "BBDaily", "BBMax",
                                                 "RSDaily", "RSMax",
                                                 "FSDaily", "FSMAx",
                                                 "JetportDaily", "JetportMax",
                                                 "Comments"),
                                   col_types = c("date", "numeric", "numeric",
                                                 "numeric","numeric",
                                                 "numeric", "numeric",
                                                 "numeric", "numeric",
                                                 "numeric", "numeric",
                                                 "text")))

  # Clean up the data
  test <- test %>%
    mutate(dd      = as.numeric(format(draftdate, format = '%d')),
           mm      = as.numeric(format(draftdate, format = '%m')),
           thedate = as.Date(paste(year, mm, dd, sep = '-'),
                           format = '%Y-%m-%d')) %>%
    select(-draftdate, -dd, -mm) %>%
    filter( ! is.na(thedate)) %>%
    select(thedate, everything())
  
  assign(dataname, test)

 # pages <- excel_sheets(fpath)
 # for (page in pages[2:12]) {
    
  #}
}
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '0.95 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.07 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '0.54 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '0.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '4.27 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A31 / R31C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B31 / R31C2: got '0.18 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D31 / R31C4: got '0.44 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F31 / R31C6: got '0.03 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H31 / R31C8: got '0.32 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J31 / R31C10: got '2.81 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got 'Indicates rain dates when an overflow
    ## event occurred'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got 'Water Equivalent for Snowfall Amounts'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F28 / R28C6: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F29 / R29C6: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F30 / R30C6: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F31 / R31C6: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F32 / R32C6: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '1.75 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.31 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '1.06 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.61 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '1.79 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '5.05 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '4.20"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '5.40"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '4.58 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '5.03 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.85 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '3.01 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.62 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.48 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.31 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '6.91 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '6.37 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '6.81 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '6.35 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '6.40"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '1.24 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.53 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '1.19 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.48 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '1.26 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '3.45 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '3.06 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '5.05 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.69 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.49 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '7.43 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '7.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '8.97 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '6.67 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '7.06 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.19 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.07 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.55 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.36 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B9 / R9C2: got a date

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '2.56 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '2.12 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '3.16 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '2.38 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '2.30 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.75 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.39 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '7.50 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '4.54 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '5.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '3.00"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.45 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.51 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.44 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.37 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A32 / R32C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B32 / R32C2: got '4.47 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D32 / R32C4: got '3.39 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F32 / R32C6: got '4.96 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H32 / R32C8: got '3.23 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J32 / R32C10: got '4.17 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got 'Indicates rain dates when an overflow
    ## event occurred'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.46 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.23 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '6.95 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '4.33 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '4.37 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '1.94 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '1.60"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '2.27 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '1.51 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '1.56 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.17 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.83 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.21 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.79 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.04 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '4.56 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '4.13 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '5.53 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '4.21 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '4.42 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.61 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.39 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.03 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.11 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '1.70"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '1.99 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.13 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.42 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.07 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '1.90"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '0.89 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '0.88 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '1.23 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '0.88 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '0.86 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '8.65 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '7.17 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '8.50"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '8.91 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '8.36 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B9 / R9C2: got a date

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '4.04 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '3.45 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '4.42 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '3.99 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '4.06 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.50 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.08 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '4.09 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '4.52 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '5.31 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '3.12 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '3.20"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.47 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.96 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.65 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A31 / R31C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B31 / R31C2: got '1.99 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D31 / R31C4: got '0.73 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F31 / R31C6: got '1.67 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H31 / R31C8: got '1.53 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J31 / R31C10: got '4.55 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got 'Indicates rain dates when an overflow
    ## event occurred'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got 'Water equivalent for snowfall amounts'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '1.95 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.81 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.06 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.50"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '6.64 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '5.11 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '7.52 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '6.28 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '6.04 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K17 / R17C11: got '.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '6.28 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '5.38 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '6.80"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '5.58 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '5.94 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '2.58 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '2.66 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '2.85 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '2.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '2.62 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '1.16 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.60"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '1.14 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.01 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '1.05 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '3.30"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.79 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '4.18 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.91 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.99 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '2.61 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '2.38 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '2.93 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '1.96 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '2.23 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B3 / R3C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B4 / R4C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B5 / R5C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B6 / R6C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B7 / R7C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B8 / R8C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B9 / R9C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B10 / R10C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B11 / R11C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B12 / R12C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B13 / R13C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B14 / R14C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B15 / R15C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B16 / R16C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B17 / R17C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B18 / R18C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B19 / R19C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B20 / R20C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B21 / R21C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B22 / R22C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B23 / R23C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B24 / R24C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B25 / R25C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B26 / R26C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B27 / R27C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B28 / R28C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B29 / R29C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B30 / R30C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B31 / R31C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B32 / R32C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '0 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.88 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '5.64 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '5.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '5.14 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B3 / R3C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H3 / R3C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B4 / R4C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H4 / R4C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B5 / R5C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H5 / R5C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B6 / R6C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H6 / R6C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B7 / R7C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H7 / R7C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B8 / R8C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H8 / R8C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B9 / R9C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H9 / R9C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E31 / R31C5: got '`'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '1.81 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '2.15 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '2.93 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '1.86 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '2.10"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D14 / R14C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D15 / R15C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D16 / R16C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D17 / R17C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D18 / R18C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D19 / R19C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D20 / R20C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D21 / R21C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D22 / R22C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D23 / R23C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D24 / R24C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D25 / R25C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D26 / R26C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D27 / R27C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D28 / R28C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D29 / R29C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D30 / R30C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D31 / R31C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D32 / R32C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.31 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '0.98 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.28 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.67 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.47 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.53 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.32 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.40"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.74 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.29 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A31 / R31C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B31 / R31C2: got '3.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D31 / R31C4: got '3.49 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F31 / R31C6: got '3.61 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H31 / R31C8: got '3.80"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J31 / R31C10: got '4.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got 'Indicates rain dates when an overflow
    ## event occurred'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got 'Water equivalent for snowfall amounts'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '1.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.56 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '1.21 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.20 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '6.39 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '5.60 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '7.22 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '5.73 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '5.83 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '0.68 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '0.66 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '0.71 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '0.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '0.79 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '3.62 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '3.49 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '3.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '3.43 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '3.30 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '3.55 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '3.89 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.37 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '3.95 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.59 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.73 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.39 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '4.50 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '3.67 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.83 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '5.75 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '5.35 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '5.48 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '5.64 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '5.71 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B17 / R17C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B18 / R18C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B19 / R19C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B20 / R20C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B21 / R21C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B22 / R22C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B23 / R23C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B24 / R24C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B25 / R25C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B26 / R26C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B27 / R27C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B28 / R28C2: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '5.20 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '5.62 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '4.54 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '4.94 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '9.56 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '9.28 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '9.74 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '8.72 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '10.26 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '3.44 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '3.29 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.15 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '3.60 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '3.65 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.28 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.62 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.58 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '4.45 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '5.53 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A31 / R31C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B31 / R31C2: got '2.87 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D31 / R31C4: got '2.81 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F31 / R31C6: got '2.91 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H31 / R31C8: got '2.64 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J31 / R31C10: got '3.95 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got 'Indicates rain dates when an overflow
    ## event occurred'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got 'Water equivalent for snowfall amounts'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.88 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '2.84 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '3.23 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '2.78 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.71 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '5.66 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '5.25 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '5.89 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '4.93 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '5.19 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.34 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.36 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '4.56 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '3.71 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '4.10 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H8 / R8C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H9 / R9C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H10 / R10C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H11 / R11C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H12 / R12C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H13 / R13C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H14 / R14C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H15 / R15C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H16 / R16C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H17 / R17C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H18 / R18C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H19 / R19C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H20 / R20C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H21 / R21C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H22 / R22C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H23 / R23C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H24 / R24C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H25 / R25C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H26 / R26C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H27 / R27C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H28 / R28C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H29 / R29C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H30 / R30C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H31 / R31C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H32 / R32C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '6.15 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '5.75 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '6.77 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '0.45 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '5.85 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H3 / R3C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H4 / R4C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H5 / R5C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H6 / R6C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H7 / R7C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H8 / R8C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H9 / R9C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H10 / R10C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H11 / R11C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H12 / R12C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D13 / R13C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H13 / R13C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D14 / R14C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H14 / R14C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D15 / R15C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H15 / R15C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D16 / R16C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H16 / R16C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D17 / R17C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H17 / R17C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D18 / R18C4: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H18 / R18C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H19 / R19C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H20 / R20C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H21 / R21C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H22 / R22C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H23 / R23C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H24 / R24C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H25 / R25C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H26 / R26C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H27 / R27C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H28 / R28C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H29 / R29C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H30 / R30C8: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '2.77 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '1.38 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '2.85 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '0 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '2.53 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '4.92 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '4.13 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '5.77 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '3.94 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '4.47 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '0.69 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '0.82 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '0.63 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '0.48 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '0.42 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '7.38 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '6.71 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '7.43 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '6.66 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '6.23 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A33 / R33C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B33 / R33C2: got '3.13 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D33 / R33C4: got '3.15 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F33 / R33C6: got '2.90 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H33 / R33C8: got '2.96 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J33 / R33C10: got '2.96 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in A34 / R34C1: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B34 / R34C2: got '5.87 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D34 / R34C4: got '5.68 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F34 / R34C6: got '5.33 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H34 / R34C8: got '5.42 "'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J34 / R34C10: got '8.48 "'

``` r
rm(test)
```

All the warnings are about the “Total” rows by month, which we wanted to
drop anyway.

# Write Rainfall Data File

## Combine Data

There aught to be a way to automate this step so I don’t have to write
out each name.

``` r
raindata <- raindata_2015 %>%
  bind_rows(raindata_2016) %>%
  bind_rows(raindata_2017) %>%
  bind_rows(raindata_2018) %>%
  bind_rows(raindata_2019) %>%
  mutate(across(! c(thedate, Comments), replace_na, 0))
rm(raindata_2015, raindata_2016, raindata_2017, raindata_2018, raindata_2019)
rm(raindata_NA)
```

    ## Warning in rm(raindata_NA): object 'raindata_NA' not found

``` r
write_csv(raindata, "portlandrainfall.csv")
```

# Load CSO Event Files

## Collect File Names

This is made slightly complicated because of inconsistent file naming
and format conventions.

``` r
rainfns <- list.files(niece, pattern = 'Rain')
allfns  <- list.files(niece)
fns <- allfns[! allfns %in% rainfns]
fns <- keep(fns, ~ file.exists(file.path(niece, .x)) &&
                   ! dir.exists(file.path(niece, .x)))
print(fns)
```

    ## [1] "2015_Portland_CSO_Overflow_Estimates.xls"           
    ## [2] "2016_Portland_Flows_-_Final.xlsx"                   
    ## [3] "2019 Portland CSO Activity and Volumes - Final.xlsx"
    ## [4] "DATA_SOURCES.md"                                    
    ## [5] "Portland_CSO_Activity_-_2017.xlsx"                  
    ## [6] "Portland_CSO_Activity_-_2018.xlsx"

### Code to Extract Year From Each Filename

This regex slight of hand searches each file name for four successive
digits, surrounded by zero or more characters on either side. Sub()
replaces the pattern match (the whole string) with “group 1” (the
matched string of four digits). In other words, it extracts the year
from the variable filename. (The group is defined by the parentheses.)

``` r
(years <- as.integer(sub('.*([0-9]{4}).*','\\1',fns)))
```

    ## Warning: NAs introduced by coercion

    ## [1] 2015 2016 2019   NA 2017 2018

## Read in 2015 Data

Remember, the 2015 data format is different, so it has to be handled
separately. (Events are not provided with a start and an end. but
reported over multiple days.)

Note also that the EEWTF wet weather flows are NOT reported by event.

Also note that there are non-numeric entries in the table which are
informative, describing managmeent of teh CSOs. Those entries will not
read correctly here. Since e they are not relevant to our analyses, that
does not matter for our current purposes, but it’s worth remembering.

### List Locations

``` r
fn    <-"2015_Portland_CSO_Overflow_Estimates.xls"
fpath <- file.path(niece, fn)

CSO_locs_15 <- read_excel(fpath, range = 'E3:AI6', col_names = FALSE)[-3,] %>%
  t() %>%
  as.tibble() %>%
  select(-1) %>%
  rename(CSO = V3, Location = V2 ) %>%
  select(CSO, Location) %>%
  mutate(CSO = sub(' ', '_', CSO))
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * ...

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
CSO_locs_15
```

    ## # A tibble: 31 x 2
    ##    CSO     Location      
    ##    <chr>   <chr>         
    ##  1 CSO_002 Arcadia St PS 
    ##  2 CSO_004 Tukey's Siphon
    ##  3 CSO_005 Randall St    
    ##  4 CSO_006 Johansen St   
    ##  5 CSO_007 Ocean Ave     
    ##  6 CSO_008 Clifton St    
    ##  7 CSO_009 George St     
    ##  8 CSO_010 Mackworth St  
    ##  9 CSO_011 Codman St     
    ## 10 CSO_012 Vannah Ave    
    ## # ... with 21 more rows

``` r
CSO_locs_15 %>%
  write_csv('Portland_Locations.csv')
```

### Read 2015 Data

``` r
CSO_data_2015 <- read_excel(fpath,  skip = 5,
                            col_types = c('numeric', 'date', 
                                          'numeric', 'numeric',         # precip
                                          rep('numeric', 31),                    # CSOs
                                          'numeric',                    # Total
                                          'skip', 'skip', 'skip', 'skip',
                                           'skip', 'skip', 'skip','skip' )) %>%
  rename(event=1, thedate = 2, totalprecip = 3, maxprecip = 4, eventtotal = 36) %>%
  filter(! is.na(thedate)) %>%
  mutate(thedate = as.Date(thedate)) %>%
  fill(event, .direction = 'down') %>%
  rename_with(~ sub(' ', '_', .))   # create syntactic names, for convenience
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E7 / R7C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I7 / R7C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K7 / R7C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M7 / R7C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S7 / R7C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U7 / R7C21: got 'REMOVED'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y7 / R7C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AI7 / R7C35: got 'PLUGGED'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E8 / R8C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I8 / R8C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K8 / R8C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M8 / R8C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S8 / R8C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y8 / R8C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E9 / R9C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I9 / R9C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K9 / R9C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M9 / R9C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S9 / R9C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T9 / R9C20: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X9 / R9C24: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E10 / R10C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I10 / R10C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K10 / R10C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M10 / R10C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S10 / R10C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T10 / R10C20: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X10 / R10C24: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y10 / R10C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E11 / R11C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I11 / R11C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K11 / R11C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M11 / R11C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S11 / R11C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T11 / R11C20: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X11 / R11C24: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y11 / R11C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E12 / R12C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I12 / R12C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K12 / R12C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M12 / R12C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S12 / R12C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T12 / R12C20: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X12 / R12C24: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y12 / R12C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E13 / R13C5: got 'BLOCK'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K13 / R13C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M13 / R13C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S13 / R13C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W13 / R13C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E14 / R14C5: got 'BLOCK'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K14 / R14C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M14 / R14C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S14 / R14C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in V14 / R14C22: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W14 / R14C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E15 / R15C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K15 / R15C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M15 / R15C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S15 / R15C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in V15 / R15C22: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W15 / R15C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E16 / R16C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K16 / R16C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M16 / R16C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S16 / R16C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W16 / R16C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E17 / R17C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K17 / R17C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M17 / R17C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S17 / R17C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W17 / R17C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E18 / R18C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K18 / R18C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M18 / R18C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W18 / R18C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E19 / R19C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K19 / R19C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M19 / R19C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W19 / R19C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E20 / R20C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I20 / R20C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K20 / R20C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M20 / R20C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W20 / R20C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E21 / R21C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I21 / R21C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K21 / R21C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M21 / R21C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W21 / R21C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E22 / R22C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I22 / R22C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K22 / R22C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M22 / R22C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W22 / R22C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E23 / R23C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I23 / R23C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K23 / R23C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M23 / R23C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W23 / R23C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E24 / R24C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I24 / R24C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K24 / R24C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M24 / R24C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N24 / R24C14: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W24 / R24C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E25 / R25C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I25 / R25C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K25 / R25C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M25 / R25C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N25 / R25C14: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W25 / R25C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E26 / R26C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I26 / R26C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K26 / R26C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M26 / R26C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W26 / R26C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E27 / R27C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I27 / R27C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K27 / R27C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M27 / R27C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W27 / R27C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E28 / R28C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I28 / R28C9: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K28 / R28C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M28 / R28C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W28 / R28C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E29 / R29C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K29 / R29C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M29 / R29C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E30 / R30C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K30 / R30C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M30 / R30C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E31 / R31C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K31 / R31C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M31 / R31C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E32 / R32C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K32 / R32C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M32 / R32C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E33 / R33C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K33 / R33C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M33 / R33C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E34 / R34C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K34 / R34C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M34 / R34C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E35 / R35C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K35 / R35C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M35 / R35C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E36 / R36C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K36 / R36C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M36 / R36C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E37 / R37C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K37 / R37C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M37 / R37C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E38 / R38C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K38 / R38C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M38 / R38C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E39 / R39C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K39 / R39C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M39 / R39C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E40 / R40C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K40 / R40C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M40 / R40C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E41 / R41C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K41 / R41C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M41 / R41C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E42 / R42C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K42 / R42C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M42 / R42C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E43 / R43C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K43 / R43C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M43 / R43C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E44 / R44C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K44 / R44C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M44 / R44C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E45 / R45C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K45 / R45C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M45 / R45C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E46 / R46C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K46 / R46C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M46 / R46C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E47 / R47C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K47 / R47C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M47 / R47C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E48 / R48C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K48 / R48C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M48 / R48C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U48 / R48C21: got 'Reinstalled 9-29'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E49 / R49C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K49 / R49C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M49 / R49C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E50 / R50C5: got 'BLOCK'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K50 / R50C11: got 'BLOCK'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M50 / R50C13: got 'BLOCK'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N50 / R50C14: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E51 / R51C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K51 / R51C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M51 / R51C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N51 / R51C14: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E52 / R52C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K52 / R52C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M52 / R52C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E53 / R53C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K53 / R53C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M53 / R53C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E54 / R54C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K54 / R54C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M54 / R54C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E55 / R55C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K55 / R55C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M55 / R55C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E56 / R56C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K56 / R56C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M56 / R56C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E57 / R57C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K57 / R57C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M57 / R57C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E58 / R58C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K58 / R58C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M58 / R58C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E59 / R59C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K59 / R59C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M59 / R59C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E60 / R60C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K60 / R60C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M60 / R60C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E61 / R61C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K61 / R61C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M61 / R61C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E62 / R62C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K62 / R62C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M62 / R62C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E63 / R63C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K63 / R63C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M63 / R63C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E64 / R64C5: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K64 / R64C11: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M64 / R64C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in B66 / R66C2: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in C66 / R66C3: got '35.11"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D66 / R66C4: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in B68 / R68C2: got 'SWMM Data'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in B69 / R69C2: got 'BLOCK = Block moved'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A70 / R70C1: got '-- Metering data unavailable'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A71 / R71C1: got '** - Runoff due to snow melt'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A74 / R74C1: got 'This data is for review purposes only and
    ## should not be considered final until further review and marked final.'

    ## New names:
    ## * INCHES -> INCHES...3
    ## * INCHES -> INCHES...4

Again, the warnings are expected, and not a problem.

### Error Correction

One date was improperly entered as from 2014. We correct that here.

``` r
CSO_data_2015$thedate[CSO_data_2015$thedate == as.Date('2014-07-20')] <- as.Date('2015-07-20')
```

### Reprocess to Match Other File Format

We use `pivot_longer()` -\> `Group_by()` -\> `summarize()`, followed by
`pivot_wider()` to simplify application of similar aggregation functions
to all the CSO locations.

#### Step 1

First, use group\_by( ) with mutate() to calculate first and last dates
and max hourly precip for each event. These columns are moved to the
front to simplify the pivots, later.

``` r
CSO_data_2015 <- CSO_data_2015 %>%
  group_by(event) %>%
  mutate(firstdate = min(thedate),
         lastdate = max(thedate),
         totalprecip = sum(totalprecip, na.rm=TRUE),
         maxprecip = max(maxprecip, na.rm=TRUE)) %>%
  ungroup() %>%
  select(event, firstdate, lastdate,    # reorder columns
         totalprecip, maxprecip,
         everything())
```

#### Step 2

Now pivot to long form.

``` r
CSO_data_2015 <- CSO_data_2015 %>%
  pivot_longer(cols = CSO_002:eventtotal,
               names_to = 'SourceCol',
               values_to = 'Vol')
```

#### Step 3

Use summarize to generate event totals. (note that leaving out the
“na.rm = TRUE” in the second to last line here led to a mysterious
mismatch between our values and the totals calculated in the source
Excel files. The reason we broke this calculation up into many pieces
was to help isolate where the error was hiding.

``` r
CSO_data_2015 <- CSO_data_2015 %>%
  group_by(event, SourceCol) %>%
  summarize(firstdate = first(firstdate),
            lastdate = last(lastdate),
            days = as.integer(lastdate - firstdate) + 1,
            totalprecip = first(totalprecip),
            maxprecip = first(maxprecip),
            total = sum(Vol, na.rm = TRUE),
            .groups = 'drop')
```

#### Step 4

Finally, pivot back to the wide form.

``` r
CSO_data_2015 <- CSO_data_2015 %>%
  pivot_wider(names_from = SourceCol, values_from = total)
```

### Which CSOs Were Monitored All Year in 2015?

Unmeasured events and CSOs are depicted in 2015 with ‘–’. We want to
look at those events and see how much of a problem they may be for our
presentation.

``` r
CSO_data_missing <- read_excel(fpath,  skip = 5,
                            col_types = c('numeric', 'date', 
                                          'skip', 'skip',         # precip
                                          rep('text', 31),              # CSOs
                                          'skip',                    # Total
                                          'skip', 'skip', 'skip', 'skip',
                                          'skip', 'skip', 'skip','skip' )) %>%
  rename(event=1, thedate = 2) %>%
  filter(! is.na(thedate)) %>%
  mutate(thedate = as.Date(thedate)) %>%
  fill(event, .direction = 'down') %>%
  rename_with(~ sub(' ', '_', .))  %>%  # create syntactic names, for convenience

  # First, use group_by( ) with mutate() to calculate first and last dates.
  group_by(event) %>%
  mutate(firstdate = min(thedate),
         lastdate = max(thedate)) %>%
  ungroup() %>%
  select(event, firstdate, lastdate,    # reorder columns
         everything()) %>%
  
  #Now pivot to long form, and summarize to flag unmeasured discharges
  pivot_longer(cols = CSO_002:CSO_043,
               names_to = 'SourceCol',
               values_to = 'Vol') %>%
  group_by(event, SourceCol) %>%
  summarize(firstdate = first(firstdate),
            lastdate = last(lastdate),
            unmeasured = any(Vol == '--' | Vol == 'BLOCK'),
            .groups = 'drop') %>%
  group_by(SourceCol) %>%
  summarize(n               = n(),
            n_unmeasured    = sum(unmeasured, na.rm=TRUE),
            pct_unmeasured = round(n_unmeasured/n,3)*100,
            .groups = 'drop')
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in B66 / R66C2: got 'Total:'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in B68 / R68C2: got 'SWMM Data'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in B69 / R69C2: got 'BLOCK = Block moved'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A70 / R70C1: got '-- Metering data unavailable'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A71 / R71C1: got '** - Runoff due to snow melt'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A74 / R74C1: got 'This data is for review purposes only and
    ## should not be considered final until further review and marked final.'

``` r
 CSO_data_missing[CSO_data_missing$pct_unmeasured>0,] %>%
   arrange(-n_unmeasured) %>%
   knitr::kable(col.names = c('CSO', 'Events', 'Unmeasured', 'Percent'))
```

| CSO      | Events | Unmeasured | Percent |
| :------- | -----: | ---------: | ------: |
| CSO\_002 |     35 |         35 |   100.0 |
| CSO\_009 |     35 |         35 |   100.0 |
| CSO\_011 |     35 |         35 |   100.0 |
| CSO\_023 |     35 |         10 |    28.6 |
| CSO\_007 |     35 |          9 |    25.7 |
| CSO\_017 |     35 |          5 |    14.3 |
| CSO\_025 |     35 |          3 |     8.6 |
| CSO\_012 |     35 |          2 |     5.7 |
| CSO\_018 |     35 |          2 |     5.7 |
| CSO\_024 |     35 |          2 |     5.7 |
| CSO\_020 |     35 |          1 |     2.9 |

## Read the Other Files

Unfortunately, the other files are not laid out quite consistently, so
while it is possible to read these files in programatically, it is
quicker (if less elegant) to just read each in separately.

### 2016 Data

``` r
fn <- "2016_Portland_Flows_-_Final.xlsx"
fpath = file.path(niece, fn)

CSO_data_2016 <- read_excel(fpath, skip = 5,
                            col_types = c('numeric', 'date', 'date',
                                          rep('numeric', 31),           # CSOs
                                          'numeric',                    # Total
                                          'numeric', 'numeric',         # precip
                                          'skip', 'skip', 'skip',
                                          'skip', 'skip')) %>%
  rename(event=1, firstdate = 2, lastdate = 3, eventtotal = 35,
         totalprecip = 36, maxprecip = 37 ) %>%
  filter(! is.na(firstdate)) %>%
  mutate(firstdate = as.Date(firstdate),
         lastdate = as.Date(lastdate),
         days = as.integer(lastdate - firstdate) + 1) %>%
  rename_with(~ sub(' ', '_', .)) %>%  # create syntactic names, for convenience)
  select(event, firstdate, lastdate, days, totalprecip, maxprecip, everything())
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D7 / R7C4: got 'BLOCK'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in C48 / R48C3: got 'TOTALS'

    ## New names:
    ## * `` -> ...1
    ## * DATE -> DATE...2
    ## * DATE -> DATE...3

\[3\] “2019 Portland CSO Activity and Volumes - Final.xlsx” \[4\]
"Portland\_CSO\_Activity\_-*2017.xlsx"  
\[5\] "Portland\_CSO\_Activity*-\_2018.xlsx"

#### Error Correction

One event was improperly entered as being from 2017. We correct that
here.

``` r
CSO_data_2016$firstdate[CSO_data_2016$lastdate == as.Date('2017-02-04')] <- as.Date('2016-02-03')
CSO_data_2016$lastdate[CSO_data_2016$lastdate  == as.Date('2017-02-04')] <- as.Date('2016-02-04')
```

### 2017 Data

CSO 43 has been dropped, probably as inactive.

There are a number of typographical errors in the date columns,
especially in the year designation in the lastdate value.

``` r
fn <- "Portland_CSO_Activity_-_2017.xlsx"
fpath = file.path(niece, fn)

CSO_data_2017 <- read_excel(fpath, skip = 6,
                            col_types = c('numeric', 'date', 'date',
                                          rep('numeric', 30),           # CSOs
                                          'numeric',                    # Total
                                          'numeric', 'numeric',         # precip
                                          'skip', 'skip', 'skip',
                                          'skip', 'skip')) %>%
  rename(event=1, firstdate = 2, lastdate = 3, eventtotal = 34,
         totalprecip = 35, maxprecip = 36 ) %>%
  filter(! is.na(firstdate)) %>%
  mutate(firstdate = as.Date(firstdate),
         lastdate  = as.Date(lastdate)) %>%
  rename_with(~ sub(' ', '_', .))   # create syntactic names, for convenience)
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in C9 / R9C3: got '1/11/207'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R14 / R14C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M15 / R15C13: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W41 / R41C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W42 / R42C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W43 / R43C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W44 / R44C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W45 / R45C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in C58 / R58C3: got 'TOTALS'

    ## New names:
    ## * `` -> ...1
    ## * DATE -> DATE...2
    ## * DATE -> DATE...3

``` r
# Error Corrections

CSO_data_2017 <- CSO_data_2017 %>%
  mutate(year  = as.integer(format(lastdate, '%Y')),
         mm = as.integer(format(lastdate, '%m')),
         dd   = as.integer(format(lastdate, '%d'))) %>%
  mutate(lastdate = if_else(year == 2007,
                            as.Date(paste(year, mm, dd, sep = '-'),
                                    format = '%Y-%m-%d'),
                            lastdate)) %>%
  select(-year, -mm, -dd)

CSO_data_2017$lastdate[2] <- as.Date('2017-01-11', format = '%Y-%m-%d')

# Assemble final data
CSO_data_2017 <- CSO_data_2017 %>%
  mutate(days = as.integer(lastdate - firstdate) + 1) %>%
  select(event, firstdate, lastdate, days, totalprecip, maxprecip, everything())
```

#### Error Correction

One date was improperly entered as being from 2007. We correct that
here.

``` r
CSO_data_2017$days[CSO_data_2017$lastdate == as.Date('2007-04-22')] <- 1
CSO_data_2017$lastdate[CSO_data_2017$lastdate == as.Date('2007-04-22')] <- as.Date('2017-04-22')
```

### 2018 Data

There is again one typographical error on the dates, where the year was
transcribed incorrectly.

``` r
fn <- "Portland_CSO_Activity_-_2018.xlsx"
fpath = file.path(niece, fn)

CSO_data_2018 <- read_excel(fpath, skip = 6,
                            col_types = c('numeric', 'date', 'date',
                                          rep('numeric', 30),           # CSOs
                                          'numeric',                    # Total
                                          'numeric', 'numeric',         # precip
                                          'skip', 'skip', 'skip',
                                          'skip', 'skip')) %>%
  rename(event=1, firstdate = 2, lastdate = 3, eventtotal = 34,
         totalprecip = 35, maxprecip = 36 ) %>%
  filter(! is.na(firstdate)) %>%
  mutate(firstdate = as.Date(firstdate),
         lastdate  = as.Date(lastdate),
         days = as.integer(lastdate - firstdate) + 1) %>%
  rename_with(~ sub(' ', '_', .)) %>% # create syntactic names, for convenience)
  select(event, firstdate, lastdate, days, totalprecip, maxprecip, everything())
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R30 / R30C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W32 / R32C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Q33 / R33C17: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T33 / R33C20: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W33 / R33C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W34 / R34C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W35 / R35C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W36 / R36C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W37 / R37C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W38 / R38C23: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF40 / R40C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF41 / R41C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF42 / R42C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF43 / R43C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF44 / R44C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF45 / R45C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF46 / R46C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF47 / R47C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF48 / R48C32: got 'Meter Removed'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AG49 / R49C33: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in C71 / R71C3: got 'TOTALS'

    ## New names:
    ## * `` -> ...1
    ## * DATE -> DATE...2
    ## * DATE -> DATE...3

``` r
# Error Correction
CSO_data_2018$lastdate[25] <- as.Date('2018-07-23', format = '%Y-%m-%d')
CSO_data_2018$days[25] <- 2
```

### 2019 Data

There is again one typogrphical error on the dates, where the year was
transcribed incorrectly.

``` r
fn <- "2019 Portland CSO Activity and Volumes - Final.xlsx"
fpath = file.path(niece, fn)

CSO_data_2019 <- read_excel(fpath, skip = 10,
                            col_types = c('numeric', 'date', 'date',
                                          rep('numeric', 30),           # CSOs
                                          'numeric',                    # Total
                                          'numeric', 'numeric',         # precip
                                          'skip', 'skip', 'skip',
                                          'skip', 'skip')) %>%
  rename(event=1, firstdate = 2, lastdate = 3, eventtotal = 34,
         totalprecip = 35, maxprecip = 36 ) %>%
  filter(! is.na(firstdate)) %>%
  mutate(firstdate = as.Date(firstdate),
         lastdate  = as.Date(lastdate),
         days = as.integer(lastdate - firstdate) + 1) %>%
  rename_with(~ sub(' ', '_', .)) %>% # create syntactic names, for convenience)
  select(event, firstdate, lastdate, days, totalprecip, maxprecip, everything())
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R15 / R15C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R16 / R16C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R20 / R20C18: got 'Removed Apr 8'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R21 / R21C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R22 / R22C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R23 / R23C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R24 / R24C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R25 / R25C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA25 / R25C27: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R26 / R26C18: got 'Reinstalled May 22'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA26 / R26C27: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA27 / R27C27: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S28 / R28C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA28 / R28C27: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S29 / R29C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA30 / R30C27: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R41 / R41C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U41 / R41C21: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R42 / R42C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U42 / R42C21: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U43 / R43C21: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R44 / R44C18: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U44 / R44C21: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S50 / R50C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S51 / R51C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S52 / R52C19: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y56 / R56C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y57 / R57C25: got '--'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting date in C58 / R58C3: got 'TOTALS'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D59 / R59C4: got 'Note 1: Discharge volume data should
    ## be reported in gallons. Discharge events lasting more than one day should show
    ## total volume discharged for each day.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D60 / R60C4: got 'Note 2: Block activity should be shown as
    ## a "1" if the block floated away.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D61 / R61C4: got 'Note 3: Data in red indicates volume
    ## after storage tank. Struckthrough results were assumed totally captured by tank.
    ## See notes for individual capture volumes.'

    ## New names:
    ## * `` -> ...1
    ## * DATE -> DATE...2
    ## * DATE -> DATE...3

#### Error Correction

One date was improperly entered as being from 3019. We correct that
here.

``` r
CSO_data_2019$days[CSO_data_2019$lastdate == as.Date('3019-06-11')] <- 1
CSO_data_2019$lastdate[CSO_data_2019$lastdate == as.Date('3019-06-11')] <- as.Date('2019-06-11')
```

#### Subtract values “Assumed Captured by Tank”

A number of values shown in the 2019 Excel data are confusing. All
numbers associated with CSO 006, and CSO 007 are in RED. SOme discharge
numbers are listed in the Excel sheet, but also shown in “strike
through”. The “strike through” values were subtracted from column
(CSO) totals, but not from row (event) totals in the source Excel File.

The failure to subtract these values them from the row totals may have
been a partial oversight by PWD Staff. Annual Total discharges were
calculated as the sum of the (corrected) column totals. It is not clear
if the row (event) totals were reviewed.

A Note at the bottom of the spreadsheet says: \> Note 3: Data in red
indicates volume after storage tank. Struckthrough results were assumed
totally captured by tank. See notes for individual capture volumes.

##### Relevant Events

| **Event** | **CSO\_006** | **CSO\_007** |
| --------- | ------------ | ------------ |
| 3         | ~~78000~~    | 2585000      |
| 12        | ~~32600~~    | 1567000      |
| 26        | ~~3200~~     |              |
| 27        | ~~200~~      |              |
| 32        | ~~2000~~     | ~~515000~~   |
| 46        | 38800        | 1247000      |

We remove all the “strike through” values here, before any calculations.

``` r
CSO_data_2019[CSO_data_2019$event ==  3,]$CSO_006 <- 0
CSO_data_2019[CSO_data_2019$event == 12,]$CSO_006 <- 0
CSO_data_2019[CSO_data_2019$event == 26,]$CSO_006 <- 0
CSO_data_2019[CSO_data_2019$event == 27,]$CSO_006 <- 0
CSO_data_2019[CSO_data_2019$event == 32,]$CSO_006 <- 0

CSO_data_2019[CSO_data_2019$event == 32,]$CSO_007 <- 0
```

#### Remove early 2020 Events

``` r
CSO_data_2019 <- CSO_data_2019 %>%
  filter(lastdate < as.Date("2020-01-01"))
```

# Save Five Years of Portland CSO Event Data

## Combine Data

There aught to be a way to automate this step so I don’t have to write
out each name.

``` r
CSO_data_15_19 <- CSO_data_2015 %>%
  bind_rows(CSO_data_2016) %>%
  bind_rows(CSO_data_2017) %>%
  bind_rows(CSO_data_2018) %>%
  bind_rows(CSO_data_2019) %>%
  mutate(Year = as.integer(format(firstdate, format = '%Y')))
rm(CSO_data_2015, CSO_data_2016, CSO_data_2017, CSO_data_2018, CSO_data_2019)
```

``` r
write_csv(CSO_data_15_19, 'Portland_CSO_data_2015_2019.csv')
```

# Export Data for GIS

## Calculate Totals by CSO

``` r
data_by_cso <- CSO_data_15_19 %>%
  summarize(across(contains('CSO'),
                   c(Events = ~ sum(! is.na(.x) & .x>0),
                   Volume = ~ sum(.x, na.rm = TRUE),
                   Events2019 = ~ sum((! is.na(.x) & .x>0) *(Year == 2019), na.rm = TRUE),
                   Volume2019 = ~ sum(.x *(Year == 2019), na.rm = TRUE)),
                   .names = paste0('{.col}_{.fn}'))) %>%
  t %>%         # t transposes, but also converts to an array, with dimnames
  tibble(Item = dimnames(.)[[1]]) %>%
  rename(Val = 1) %>%
  mutate(group = sub('^.*_', '', Item),
         CSO   = substr(Item, 1,7)) %>%
  pivot_wider(id_cols = CSO, names_from = group, values_from = Val) %>%
  inner_join(CSO_locs_15, by = 'CSO') %>%
  select(CSO, Location, everything())
```

## Export File

``` r
write_csv(data_by_cso, 'portland_cso_summary.csv')
```
