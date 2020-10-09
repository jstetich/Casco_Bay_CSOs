Casco Bay CSO Exploratory Analysis and Graphics
================
Curtis C. Bohlen, Casco Bay Estuary Partnership

  - [Load DEP Data](#load-dep-data)
      - [Establish Folder References](#establish-folder-references)
  - [Load Weather Data](#load-weather-data)
      - [Establish Folder Reference](#establish-folder-reference)
      - [Access data](#access-data)
      - [Examine Correlations](#examine-correlations)
  - [Refine Data](#refine-data)
      - [Identify Casco Bay CSO Communities by
        Name](#identify-casco-bay-cso-communities-by-name)
      - [Casco Bay Towns Data](#casco-bay-towns-data)
      - [Totals Data](#totals-data)
  - [Merge Three Data Sets](#merge-three-data-sets)
  - [Correlations by Year](#correlations-by-year)
  - [Generate Models and Extract
    Coefficients](#generate-models-and-extract-coefficients)
      - [Calculate Annual Percent
        Changes](#calculate-annual-percent-changes)
      - [The Overall 23-Year Slope](#the-overall-23-year-slope)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

\#Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages -------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.2     v purrr   0.3.4
#> v tibble  3.0.3     v dplyr   1.0.2
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())

library(corrplot)
#> corrplot 0.84 loaded

library(mblm)  # Includes a Theil-Sen estimator.
               # See also archive on atmospheric deposition (?)
               # or maybe it was bacteris in shellfish....)
```

# Load DEP Data

## Establish Folder References

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

``` r
fn <-'DEP_Annual_Totals.csv'
fpath <- file.path(sibling, fn)
the_data <- read_csv(fpath, col_types = 
                       c(Community = col_character(),
                         Year = col_integer(),
                         Volume = col_double(),
                         Events = col_double(),
                         Outfalls = col_double()))
```

# Load Weather Data

## Establish Folder Reference

``` r
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
```

## Access data

We extract annual Precipitation Totals (in mm), and Annual Days with
more than one tenth of an inch (2.5mm), and one inch (25.4mm) of rain
from the annual weather summaries from NOAA.

``` r
fn <-'Annual_Weather_PWD.csv'
fpath <- file.path(sibling, fn)
rain_data <- read_csv(fpath, col_types =
                       cols(date = col_datetime(format = ""),
                            datatype = col_character(),
                            value = col_double(),
                            attributes = col_character(),
                            station = col_skip())) %>%
  mutate(Year = as.integer(format(date, format = '%Y'))) %>%
  filter (datatype %in% c('PRCP', 'DP10', 'DP1X')) %>%
  select(Year, datatype, value) %>%
  pivot_wider(names_from = datatype, values_from = value) %>%
  rename(Precip_mm = PRCP, GT0.1 = DP10, GT1.0 = DP1X) %>%
  mutate(Precip_in = Precip_mm / 25.4) %>%
  filter(Year > 1996)
```

## Examine Correlations

``` r
cor(rain_data, use = 'pairwise')
#>                Year     GT0.1     GT1.0 Precip_mm Precip_in
#> Year      1.0000000 0.2747309 0.2421001 0.2748405 0.2748405
#> GT0.1     0.2747309 1.0000000 0.5995589 0.7370457 0.7370457
#> GT1.0     0.2421001 0.5995589 1.0000000 0.8812085 0.8812085
#> Precip_mm 0.2748405 0.7370457 0.8812085 1.0000000 1.0000000
#> Precip_in 0.2748405 0.7370457 0.8812085 1.0000000 1.0000000
```

There are what are likely to be statistically significant correlations
between year and number of large storm and total rainfall even over this
short period. (See the Climate Change indicator for analysis of a longer
time series.)

The high correlation between number of large storms and total
precipitation may complicate separating those two factors in any
analysis – but we don’t really need to do that in this worksheet.

# Refine Data

## Identify Casco Bay CSO Communities by Name

``` r
cb_cso_towns <- c("Cape Elizabeth",
                "Portland & PWD",
                "South Portland",
                "Westbrook",
                "Yarmouth")            
```

## Casco Bay Towns Data

Although the number of events and outfalls were reported in the 2008 CSO
report for years prior to 2006, they show no variation, suggesting these
data are not meaningful, so we strip them out here.

``` r
cb_towns_data_long <- the_data %>%
  filter(Community %in% cb_cso_towns) %>%
  mutate(VolumeMG = Volume / (10^6)) %>%
  filter(Year > 1996)

cb_towns_data_wide <- cb_towns_data_long %>%
  pivot_wider(Year, names_from = Community,
              values_from = c(Volume, Events, Outfalls),
              names_repair = 'universal') %>%
  rename_with(~ sub('\\.\\.\\.', '.', .x )) %>% # replace triple dots in Portland
  rowwise() %>%
  mutate(Total = sum(c(Volume_Cape.Elizabeth,
                       Volume_Portland.PWD,
                       Volume_South.Portland,
                       Volume_Westbrook,
                       Volume_Yarmouth), na.rm = TRUE),
         PctPortland = Volume_Portland.PWD/Total) %>%
  ungroup()
#> New names:
#> * `Events_Cape Elizabeth` -> Events_Cape.Elizabeth
#> * `Events_Portland & PWD` -> Events_Portland...PWD
#> * `Events_South Portland` -> Events_South.Portland
#> * `Outfalls_Cape Elizabeth` -> Outfalls_Cape.Elizabeth
#> * `Outfalls_Portland & PWD` -> Outfalls_Portland...PWD
#> * ...
```

## Totals Data

Including total discharge volumes each year, in Millions of Gallons.

``` r
annual_data <- the_data %>%
  group_by(Year) %>%
  summarize(TotVol      = sum(Volume,   na.rm = TRUE),
            TotVolMG    = TotVol / (10^6),
            TotEvents   = sum(Events,   na.rm = TRUE),
            TotOutfalls = sum(Outfalls, na.rm = TRUE),
            
            CBTotVol      = sum(Volume*(Community %in% cb_cso_towns),
                                na.rm = TRUE),
            CBVolMG    = CBTotVol / (10^6),
            CBTotEvents   = sum(Events*(Community %in% cb_cso_towns),
                                na.rm = TRUE),
            CBTotOutfalls = sum(Outfalls*(Community %in% cb_cso_towns),
                                na.rm = TRUE),
            
            CBPctVol       = round(CBTotVol / TotVol, 4) * 100,
            CBPctEvents    = round(CBTotEvents / TotEvents, 4) * 100,
            CBPctOutfalls  = round(CBTotOutfalls / TotOutfalls, 4) * 100,
            .groups = 'drop') %>%
  filter(Year > 1996)
```

# Merge Three Data Sets

``` r
annual_data_all <- rain_data %>%
  left_join(cb_towns_data_wide, by = 'Year') %>%
  select (-Total) %>%
  rename(PctCascoPortland = PctPortland) %>%
  left_join(annual_data, by = 'Year') %>%
  mutate(CSO_MG_per_inch = CBVolMG / Precip_in)
rm(annual_data, cb_towns_data_wide, rain_data, the_data)
```

# Correlations by Year

``` r
cors <- cor(annual_data_all, use = 'pairwise')
#> Warning in cor(annual_data_all, use = "pairwise"): the standard deviation is
#> zero
corrplot(cors, method="circle", type = 'upper', tl.cex = .5)
```

![](DEP_CSO_Analysis_files/figure-gfm/correlation_figure-1.png)<!-- -->

``` r
rm(cors)
```

What jumps out is:

1.  Year is negatively correlated with volume and events
2.  Year is positively (though not strongly) correlated with
    precipitation.
3.  Total CSO VOlumes and Events statewide are dropping even faster than
    in our region, so the percentage of the states CSOs from her is
    climbing.
4.  Volume and number of CSO events are correlated across the region.
    Bad years in one jurisdiction are bad for many.
5.  Portland’s CSOs are a big enough part of regional and state-wide
    totals so that they are always highly correlated with totals. The
    precipitation variables are not all that highly correlated with the
    other variables, but that

# Generate Models and Extract Coefficients

We want to extract slopes for the reduction in CSO volumes for each of
our communities, and express them as an annual average percent reduction
in CSO volume over the last two decades. Since Yarmouth is no longer
considered a CSO community, we exclude them from the analysis.

Given the high variability of CSO discharge numbers, and the strongly
non-normal structure of errors, it is unlikely that standard linear
regressions would be suited to documenting the rate of reduction in CSO
volumes. Here wqe calculate both linear and Theil-Sen estimators of
slope on

Here we use tidyr’s list columns to automate identical statistical
analyses for each town.

We use a log transformed analysis, rather than a linear analysis, as it
better addresses the structure of the data, and allows us to readily
reinterpret teh model coefficients in terms of an annual long-term
average decline in volumes.

Statistical significance of a (bivariate) Theil-Sen slope estimate is
identical to the test for significance of the closely-related Kendall’s
Tau correlation coefficient. We use `cor.test()` to test for
significance. We do not extract the p value of the linear models, as we
would not believe them anyway, but we do look at the related t
statistic, to help get a sense of the strength of the evidence
supporting a linear trend. Teh two methoids give similar assessmsnts of
which slopes are robust, although slopeestimates differ somewhat.

We report the Theil-Sen slopes.

``` r
res <- cb_towns_data_long %>%
  filter(Community != 'Yarmouth') %>%
  mutate(logVolume = log(Volume+100)) %>%
  group_by(Community) %>%
  nest() %>%
  mutate(mylm = map(data, function(df) lm(logVolume ~ Year, data = df)),
         lmslope = unlist(map(mylm, function(m) coef(m)[2])),
         lmsd = unlist(map(mylm,
                             function(m) coef(summary(m))[, 2][[2]])),
         tlm = lmslope/lmsd,
         myts = map(data, function(df) mblm(logVolume ~ Year, data = df)),
         tsslope = unlist(map(myts, function(m) coef(m)[2])),
         tsCortest = (map(data, function(df)
                     cor.test(df$Year, df$logVolume, method = 'kendall'))),
         tscor = unlist(map(tsCortest, function(x) x$estimate)),
         tsp = unlist(map(tsCortest, function(x) x$p.value))) %>%
  select(-tsCortest)
#> Warning: Problem with `mutate()` input `tsCortest`.
#> i Cannot compute exact p-value with ties
#> i Input `tsCortest` is `(map(data, function(df) cor.test(df$Year, df$logVolume, method = "kendall")))`.
#> i The error occurred in group 1: Community = "Cape Elizabeth".
#> Warning in cor.test.default(df$Year, df$logVolume, method = "kendall"): Cannot
#> compute exact p-value with ties
res
#> # A tibble: 4 x 10
#> # Groups:   Community [4]
#>   Community   data      mylm  lmslope   lmsd    tlm myts  tsslope  tscor     tsp
#>   <chr>       <list>    <lis>   <dbl>  <dbl>  <dbl> <lis>   <dbl>  <dbl>   <dbl>
#> 1 Cape Eliza~ <tibble ~ <lm>  -0.0405 0.0766 -0.529 <mbl~ -0.106  -0.323 3.21e-2
#> 2 Portland &~ <tibble ~ <lm>  -0.0725 0.0158 -4.58  <mbl~ -0.0853 -0.502 5.57e-4
#> 3 South Port~ <tibble ~ <lm>  -0.0981 0.0275 -3.57  <mbl~ -0.0853 -0.470 1.32e-3
#> 4 Westbrook   <tibble ~ <lm>   0.0108 0.0625  0.172 <mbl~ -0.0390 -0.138 3.73e-1
```

## Calculate Annual Percent Changes

The relationship between log-transformed variables and percent change is
simply the following:

\[\frac{y(t+1)-y(t))}{y(t)}=e^m-1\] So, we can summarize the model
results as follows:

``` r
res %>%
  select(Community, lmslope, tsslope) %>%
  mutate(across(c(lmslope, tsslope), ~ (exp(.)-1) * 100)) %>%
  rename(Ann_Pct_Chng_lm = lmslope,
         Ann_Pct_Chng_ts = tsslope)
#> # A tibble: 4 x 3
#> # Groups:   Community [4]
#>   Community      Ann_Pct_Chng_lm Ann_Pct_Chng_ts
#>   <chr>                    <dbl>           <dbl>
#> 1 Cape Elizabeth           -3.97          -10.1 
#> 2 Portland & PWD           -6.99           -8.18
#> 3 South Portland           -9.35           -8.17
#> 4 Westbrook                 1.08           -3.82
```

Here, we prefer the Theil-Sen slopes, as they are robust to outliers.

## The Overall 23-Year Slope

``` r
tmp  <- cb_towns_data_long %>%
  select(Year, Volume) %>%
  group_by(Year) %>%
  mutate(Total = sum(Volume, na.rm = TRUE)) %>%
  mutate(logVolume = log(Total + 100))        # + 100 for consistency
                                              # but not really needed here
  
overalllm <-  lm(logVolume ~ Year, data = tmp)
lmslope   <-  coef(overalllm)[2]
lmsd      <-  coef(summary(overalllm))[, 2][[2]]
tlm       <-  lmslope/lmsd


overallts <- mblm(logVolume ~ Year, data = tmp)
tsslope   <- coef(overallts)[2]

tsCortest <- cor.test(tmp$Year, tmp$logVolume, method = 'kendall')
tscor     <-  tsCortest$estimate
tsp       <-  tsCortest$p.value

cat('Linear Model Slope = ', lmslope, '\n')
#> Linear Model Slope =  -0.0738661
cat('t Statistic = ', tlm, '\n')
#> t Statistic =  -11.00852
cat('Related estimate of Annual Percent Change =',
    (exp(lmslope)-1) * 100, '\n')
#> Related estimate of Annual Percent Change = -7.120395
cat('Theil-Sen Slope = ', tsslope, '\n')
#> Theil-Sen Slope =  -0.07749891
cat('Related p value = ', tsp)
#> Related p value =  2.126838e-15
cat('Related estimate of Annual Percent Change =',
    (exp(tsslope)-1) * 100, '\n')
#> Related estimate of Annual Percent Change = -7.457197
```
