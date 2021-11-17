hw5
================
Matthew Spotnitz
11/14/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
```

\#\#Problem 1

``` r
homicide_df = read.csv("homicide-data.csv")
head(homicide_df)
```

    ##          uid reported_date victim_last victim_first victim_race victim_age
    ## 1 Alb-000001      20100504      GARCIA         JUAN    Hispanic         78
    ## 2 Alb-000002      20100216     MONTOYA      CAMERON    Hispanic         17
    ## 3 Alb-000003      20100601 SATTERFIELD      VIVIANA       White         15
    ## 4 Alb-000004      20100101    MENDIOLA       CARLOS    Hispanic         32
    ## 5 Alb-000005      20100102        MULA       VIVIAN       White         72
    ## 6 Alb-000006      20100126        BOOK    GERALDINE       White         91
    ##   victim_sex        city state      lat       lon           disposition
    ## 1       Male Albuquerque    NM 35.09579 -106.5386 Closed without arrest
    ## 2       Male Albuquerque    NM 35.05681 -106.7153      Closed by arrest
    ## 3     Female Albuquerque    NM 35.08609 -106.6956 Closed without arrest
    ## 4       Male Albuquerque    NM 35.07849 -106.5561      Closed by arrest
    ## 5     Female Albuquerque    NM 35.13036 -106.5810 Closed without arrest
    ## 6     Female Albuquerque    NM 35.15111 -106.5378        Open/No arrest

``` r
tail(homicide_df)
```

    ##              uid reported_date victim_last victim_first victim_race victim_age
    ## 52174 Was-001379      20160715      HARRIS       SHAROD       Black         20
    ## 52175 Was-001380      20160908    WILLIAMS         EVAN       Black         29
    ## 52176 Was-001381      20160913       SMITH         DEON       Black         19
    ## 52177 Was-001382      20161114  WASHINGTON       WILLIE       Black         23
    ## 52178 Was-001383      20161130      BARNES       MARCUS       Black         24
    ## 52179 Was-001384      20160901     JACKSON        KEVIN       Black         17
    ##       victim_sex       city state      lat       lon      disposition
    ## 52174       Male Washington    DC 38.82727 -77.00157   Open/No arrest
    ## 52175       Male Washington    DC 38.82870 -77.00207 Closed by arrest
    ## 52176       Male Washington    DC 38.82285 -77.00173   Open/No arrest
    ## 52177       Male Washington    DC 38.82802 -77.00251   Open/No arrest
    ## 52178       Male Washington    DC 38.82048 -77.00864   Open/No arrest
    ## 52179       Male Washington    DC 38.86669 -76.98241 Closed by arrest

``` r
str(homicide_df)
```

    ## 'data.frame':    52179 obs. of  12 variables:
    ##  $ uid          : chr  "Alb-000001" "Alb-000002" "Alb-000003" "Alb-000004" ...
    ##  $ reported_date: int  20100504 20100216 20100601 20100101 20100102 20100126 20100127 20100127 20100130 20100210 ...
    ##  $ victim_last  : chr  "GARCIA" "MONTOYA" "SATTERFIELD" "MENDIOLA" ...
    ##  $ victim_first : chr  "JUAN" "CAMERON" "VIVIANA" "CARLOS" ...
    ##  $ victim_race  : chr  "Hispanic" "Hispanic" "White" "Hispanic" ...
    ##  $ victim_age   : chr  "78" "17" "15" "32" ...
    ##  $ victim_sex   : chr  "Male" "Male" "Female" "Male" ...
    ##  $ city         : chr  "Albuquerque" "Albuquerque" "Albuquerque" "Albuquerque" ...
    ##  $ state        : chr  "NM" "NM" "NM" "NM" ...
    ##  $ lat          : num  35.1 35.1 35.1 35.1 35.1 ...
    ##  $ lon          : num  -107 -107 -107 -107 -107 ...
    ##  $ disposition  : chr  "Closed without arrest" "Closed by arrest" "Closed without arrest" "Closed by arrest" ...

``` r
view(homicide_df)
```

This data set consists of 52179 observations and 12 variables. Of those
variables, all are characters except for “lat” and “lon”, which are
integers.

``` r
homicide_df = janitor::clean_names(homicide_df)
homicide_df = homicide_df %>% mutate (
  city_state = str_c(city, state),
  resolution = case_when(
    disposition == "Closed without arrest" ~ "unsolved",
    disposition == "Open/No arrest" ~ "unsolved",
    disposition == "Closed by arrest" ~ "solved" 
  )) %>% 
  relocate(city_state) %>% 
  filter(city_state != "TulsaAL")
```

Now I will focus on Baltimore, MD

``` r
baltimore_df = homicide_df %>% filter(city_state == "BaltimoreMD")

baltimore_summary = baltimore_df %>% summarize(
  unsolved = sum(resolution == "unsolved"), n = n()
) 

baltimore_test = prop.test(
  x = baltimore_summary %>% pull(unsolved),
  n = baltimore_summary %>% pull(n)
)

baltimore_test %>% broom::tidy()
```

    ## # A tibble: 1 × 8
    ##   estimate statistic  p.value parameter conf.low conf.high method    alternative
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
    ## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample… two.sided

Now I will iterate this function.

``` r
prop_test_function = function(city_df) {
  city_summary = 
    city_df %>% 
    summarize(
      unsolved = sum(resolution == "unsolved"), n = n()
) 

city_test = prop.test(
  x = city_summary %>% pull(unsolved),
  n = city_summary %>% pull(n))
  
  return(city_test)

 
}

prop_test_function(baltimore_df)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
    ## X-squared = 239.01, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.6275625 0.6631599
    ## sample estimates:
    ##         p 
    ## 0.6455607

``` r
homicide_df %>%
  filter(city_state == "AlbuquerqueNM") %>% 
  prop_test_function()
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
    ## X-squared = 19.114, df = 1, p-value = 1.232e-05
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3372604 0.4375766
    ## sample estimates:
    ##         p 
    ## 0.3862434

Now, I will iterate across all cities.

``` r
results_df =
  homicide_df %>% 
  nest(data = uid:resolution) %>% 
  mutate(
    test_results = map(data, prop_test_function),
    tidy_results = map(test_results, broom::tidy)
  ) %>% 
  select(city_state, tidy_results) %>% 
  unnest  (tidy_results) %>% 
  select(city_state, estimate, starts_with("conf"))
```

Now I will make a plot showing estimates and confidence intervals

``` r
results_df %>% 
  mutate(city_state = fct_reorder(city_state, estimate)) %>% 
  ggplot(aes(x = city_state, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax = conf.high)) +
  theme(axis.text.x = element_text(angle = 90, vjust= 0.5, hjust = 1))
```

![](hw5_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> \#\#Problem 2

``` r
df_files = tibble(
  files = list.files("./data/zip_data/", pattern="*.csv", full.names=TRUE),
)
head(df_files)
```

    ## # A tibble: 6 × 1
    ##   files                      
    ##   <chr>                      
    ## 1 ./data/zip_data//con_01.csv
    ## 2 ./data/zip_data//con_02.csv
    ## 3 ./data/zip_data//con_03.csv
    ## 4 ./data/zip_data//con_04.csv
    ## 5 ./data/zip_data//con_05.csv
    ## 6 ./data/zip_data//con_06.csv

``` r
for (i in df_files) {
  read_files = purrr::map(i, read.csv)
  tidy_results = purrr::map(read_files, broom::tidy)
}
```

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: `data_frame()` was deprecated in tibble 1.1.0.
    ## Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

    ## Warning: Data frame tidiers are deprecated and will be removed in an upcoming
    ## release of broom.

``` r
print(tidy_results)
```

    ## [[1]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  0.2     NA   0.2     0.2      0  0.2   0.2      0   NaN      NaN
    ## 2 week_2     1 -1.31    NA  -1.31   -1.31     0 -1.31 -1.31     0   NaN      NaN
    ## 3 week_3     1  0.66    NA   0.66    0.66     0  0.66  0.66     0   NaN      NaN
    ## 4 week_4     1  1.96    NA   1.96    1.96     0  1.96  1.96     0   NaN      NaN
    ## 5 week_5     1  0.23    NA   0.23    0.23     0  0.23  0.23     0   NaN      NaN
    ## 6 week_6     1  1.09    NA   1.09    1.09     0  1.09  1.09     0   NaN      NaN
    ## 7 week_7     1  0.05    NA   0.05    0.05     0  0.05  0.05     0   NaN      NaN
    ## 8 week_8     1  1.94    NA   1.94    1.94     0  1.94  1.94     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[2]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  1.13    NA   1.13    1.13     0  1.13  1.13     0   NaN      NaN
    ## 2 week_2     1 -0.88    NA  -0.88   -0.88     0 -0.88 -0.88     0   NaN      NaN
    ## 3 week_3     1  1.07    NA   1.07    1.07     0  1.07  1.07     0   NaN      NaN
    ## 4 week_4     1  0.17    NA   0.17    0.17     0  0.17  0.17     0   NaN      NaN
    ## 5 week_5     1 -0.83    NA  -0.83   -0.83     0 -0.83 -0.83     0   NaN      NaN
    ## 6 week_6     1 -0.31    NA  -0.31   -0.31     0 -0.31 -0.31     0   NaN      NaN
    ## 7 week_7     1  1.58    NA   1.58    1.58     0  1.58  1.58     0   NaN      NaN
    ## 8 week_8     1  0.44    NA   0.44    0.44     0  0.44  0.44     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[3]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  1.77    NA   1.77    1.77     0  1.77  1.77     0   NaN      NaN
    ## 2 week_2     1  3.11    NA   3.11    3.11     0  3.11  3.11     0   NaN      NaN
    ## 3 week_3     1  2.22    NA   2.22    2.22     0  2.22  2.22     0   NaN      NaN
    ## 4 week_4     1  3.26    NA   3.26    3.26     0  3.26  3.26     0   NaN      NaN
    ## 5 week_5     1  3.31    NA   3.31    3.31     0  3.31  3.31     0   NaN      NaN
    ## 6 week_6     1  0.89    NA   0.89    0.89     0  0.89  0.89     0   NaN      NaN
    ## 7 week_7     1  1.88    NA   1.88    1.88     0  1.88  1.88     0   NaN      NaN
    ## 8 week_8     1  1.01    NA   1.01    1.01     0  1.01  1.01     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[4]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  1.04    NA   1.04    1.04     0  1.04  1.04     0   NaN      NaN
    ## 2 week_2     1  3.66    NA   3.66    3.66     0  3.66  3.66     0   NaN      NaN
    ## 3 week_3     1  1.22    NA   1.22    1.22     0  1.22  1.22     0   NaN      NaN
    ## 4 week_4     1  2.33    NA   2.33    2.33     0  2.33  2.33     0   NaN      NaN
    ## 5 week_5     1  1.47    NA   1.47    1.47     0  1.47  1.47     0   NaN      NaN
    ## 6 week_6     1  2.7     NA   2.7     2.7      0  2.7   2.7      0   NaN      NaN
    ## 7 week_7     1  1.87    NA   1.87    1.87     0  1.87  1.87     0   NaN      NaN
    ## 8 week_8     1  1.66    NA   1.66    1.66     0  1.66  1.66     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[5]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  0.47    NA   0.47    0.47     0  0.47  0.47     0   NaN      NaN
    ## 2 week_2     1 -0.58    NA  -0.58   -0.58     0 -0.58 -0.58     0   NaN      NaN
    ## 3 week_3     1 -0.09    NA  -0.09   -0.09     0 -0.09 -0.09     0   NaN      NaN
    ## 4 week_4     1 -1.37    NA  -1.37   -1.37     0 -1.37 -1.37     0   NaN      NaN
    ## 5 week_5     1 -0.32    NA  -0.32   -0.32     0 -0.32 -0.32     0   NaN      NaN
    ## 6 week_6     1 -2.17    NA  -2.17   -2.17     0 -2.17 -2.17     0   NaN      NaN
    ## 7 week_7     1  0.45    NA   0.45    0.45     0  0.45  0.45     0   NaN      NaN
    ## 8 week_8     1  0.48    NA   0.48    0.48     0  0.48  0.48     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[6]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  2.37    NA   2.37    2.37     0  2.37  2.37     0   NaN      NaN
    ## 2 week_2     1  2.5     NA   2.5     2.5      0  2.5   2.5      0   NaN      NaN
    ## 3 week_3     1  1.59    NA   1.59    1.59     0  1.59  1.59     0   NaN      NaN
    ## 4 week_4     1 -0.16    NA  -0.16   -0.16     0 -0.16 -0.16     0   NaN      NaN
    ## 5 week_5     1  2.08    NA   2.08    2.08     0  2.08  2.08     0   NaN      NaN
    ## 6 week_6     1  3.07    NA   3.07    3.07     0  3.07  3.07     0   NaN      NaN
    ## 7 week_7     1  0.78    NA   0.78    0.78     0  0.78  0.78     0   NaN      NaN
    ## 8 week_8     1  2.35    NA   2.35    2.35     0  2.35  2.35     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[7]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  0.03    NA   0.03    0.03     0  0.03  0.03     0   NaN      NaN
    ## 2 week_2     1  1.21    NA   1.21    1.21     0  1.21  1.21     0   NaN      NaN
    ## 3 week_3     1  1.13    NA   1.13    1.13     0  1.13  1.13     0   NaN      NaN
    ## 4 week_4     1  0.64    NA   0.64    0.64     0  0.64  0.64     0   NaN      NaN
    ## 5 week_5     1  0.49    NA   0.49    0.49     0  0.49  0.49     0   NaN      NaN
    ## 6 week_6     1 -0.12    NA  -0.12   -0.12     0 -0.12 -0.12     0   NaN      NaN
    ## 7 week_7     1 -0.07    NA  -0.07   -0.07     0 -0.07 -0.07     0   NaN      NaN
    ## 8 week_8     1  0.46    NA   0.46    0.46     0  0.46  0.46     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[8]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1 -0.08    NA  -0.08   -0.08     0 -0.08 -0.08     0   NaN      NaN
    ## 2 week_2     1  1.42    NA   1.42    1.42     0  1.42  1.42     0   NaN      NaN
    ## 3 week_3     1  0.09    NA   0.09    0.09     0  0.09  0.09     0   NaN      NaN
    ## 4 week_4     1  0.36    NA   0.36    0.36     0  0.36  0.36     0   NaN      NaN
    ## 5 week_5     1  1.18    NA   1.18    1.18     0  1.18  1.18     0   NaN      NaN
    ## 6 week_6     1 -1.16    NA  -1.16   -1.16     0 -1.16 -1.16     0   NaN      NaN
    ## 7 week_7     1  0.33    NA   0.33    0.33     0  0.33  0.33     0   NaN      NaN
    ## 8 week_8     1 -0.44    NA  -0.44   -0.44     0 -0.44 -0.44     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[9]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  0.08    NA   0.08    0.08     0  0.08  0.08     0   NaN      NaN
    ## 2 week_2     1  1.24    NA   1.24    1.24     0  1.24  1.24     0   NaN      NaN
    ## 3 week_3     1  1.44    NA   1.44    1.44     0  1.44  1.44     0   NaN      NaN
    ## 4 week_4     1  0.41    NA   0.41    0.41     0  0.41  0.41     0   NaN      NaN
    ## 5 week_5     1  0.95    NA   0.95    0.95     0  0.95  0.95     0   NaN      NaN
    ## 6 week_6     1  2.75    NA   2.75    2.75     0  2.75  2.75     0   NaN      NaN
    ## 7 week_7     1  0.3     NA   0.3     0.3      0  0.3   0.3      0   NaN      NaN
    ## 8 week_8     1  0.03    NA   0.03    0.03     0  0.03  0.03     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[10]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  2.14    NA   2.14    2.14     0  2.14  2.14     0   NaN      NaN
    ## 2 week_2     1  1.15    NA   1.15    1.15     0  1.15  1.15     0   NaN      NaN
    ## 3 week_3     1  2.52    NA   2.52    2.52     0  2.52  2.52     0   NaN      NaN
    ## 4 week_4     1  3.44    NA   3.44    3.44     0  3.44  3.44     0   NaN      NaN
    ## 5 week_5     1  4.26    NA   4.26    4.26     0  4.26  4.26     0   NaN      NaN
    ## 6 week_6     1  0.97    NA   0.97    0.97     0  0.97  0.97     0   NaN      NaN
    ## 7 week_7     1  2.73    NA   2.73    2.73     0  2.73  2.73     0   NaN      NaN
    ## 8 week_8     1 -0.53    NA  -0.53   -0.53     0 -0.53 -0.53     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[11]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  3.05    NA   3.05    3.05     0  3.05  3.05     0   NaN      NaN
    ## 2 week_2     1  3.67    NA   3.67    3.67     0  3.67  3.67     0   NaN      NaN
    ## 3 week_3     1  4.84    NA   4.84    4.84     0  4.84  4.84     0   NaN      NaN
    ## 4 week_4     1  5.8     NA   5.8     5.8      0  5.8   5.8      0   NaN      NaN
    ## 5 week_5     1  6.33    NA   6.33    6.33     0  6.33  6.33     0   NaN      NaN
    ## 6 week_6     1  5.46    NA   5.46    5.46     0  5.46  5.46     0   NaN      NaN
    ## 7 week_7     1  6.38    NA   6.38    6.38     0  6.38  6.38     0   NaN      NaN
    ## 8 week_8     1  5.91    NA   5.91    5.91     0  5.91  5.91     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[12]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1 -0.84    NA  -0.84   -0.84     0 -0.84 -0.84     0   NaN      NaN
    ## 2 week_2     1  2.63    NA   2.63    2.63     0  2.63  2.63     0   NaN      NaN
    ## 3 week_3     1  1.64    NA   1.64    1.64     0  1.64  1.64     0   NaN      NaN
    ## 4 week_4     1  2.58    NA   2.58    2.58     0  2.58  2.58     0   NaN      NaN
    ## 5 week_5     1  1.24    NA   1.24    1.24     0  1.24  1.24     0   NaN      NaN
    ## 6 week_6     1  2.32    NA   2.32    2.32     0  2.32  2.32     0   NaN      NaN
    ## 7 week_7     1  3.11    NA   3.11    3.11     0  3.11  3.11     0   NaN      NaN
    ## 8 week_8     1  3.78    NA   3.78    3.78     0  3.78  3.78     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[13]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  2.15    NA   2.15    2.15     0  2.15  2.15     0   NaN      NaN
    ## 2 week_2     1  2.08    NA   2.08    2.08     0  2.08  2.08     0   NaN      NaN
    ## 3 week_3     1  1.82    NA   1.82    1.82     0  1.82  1.82     0   NaN      NaN
    ## 4 week_4     1  2.84    NA   2.84    2.84     0  2.84  2.84     0   NaN      NaN
    ## 5 week_5     1  3.36    NA   3.36    3.36     0  3.36  3.36     0   NaN      NaN
    ## 6 week_6     1  3.61    NA   3.61    3.61     0  3.61  3.61     0   NaN      NaN
    ## 7 week_7     1  3.37    NA   3.37    3.37     0  3.37  3.37     0   NaN      NaN
    ## 8 week_8     1  3.74    NA   3.74    3.74     0  3.74  3.74     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[14]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1 -0.62    NA  -0.62   -0.62     0 -0.62 -0.62     0   NaN      NaN
    ## 2 week_2     1  2.54    NA   2.54    2.54     0  2.54  2.54     0   NaN      NaN
    ## 3 week_3     1  3.78    NA   3.78    3.78     0  3.78  3.78     0   NaN      NaN
    ## 4 week_4     1  2.73    NA   2.73    2.73     0  2.73  2.73     0   NaN      NaN
    ## 5 week_5     1  4.49    NA   4.49    4.49     0  4.49  4.49     0   NaN      NaN
    ## 6 week_6     1  5.82    NA   5.82    5.82     0  5.82  5.82     0   NaN      NaN
    ## 7 week_7     1  6       NA   6       6        0  6     6        0   NaN      NaN
    ## 8 week_8     1  6.49    NA   6.49    6.49     0  6.49  6.49     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[15]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  0.7     NA   0.7     0.7      0  0.7   0.7      0   NaN      NaN
    ## 2 week_2     1  3.33    NA   3.33    3.33     0  3.33  3.33     0   NaN      NaN
    ## 3 week_3     1  5.34    NA   5.34    5.34     0  5.34  5.34     0   NaN      NaN
    ## 4 week_4     1  5.57    NA   5.57    5.57     0  5.57  5.57     0   NaN      NaN
    ## 5 week_5     1  6.9     NA   6.9     6.9      0  6.9   6.9      0   NaN      NaN
    ## 6 week_6     1  6.66    NA   6.66    6.66     0  6.66  6.66     0   NaN      NaN
    ## 7 week_7     1  6.24    NA   6.24    6.24     0  6.24  6.24     0   NaN      NaN
    ## 8 week_8     1  6.95    NA   6.95    6.95     0  6.95  6.95     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[16]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  3.73    NA   3.73    3.73     0  3.73  3.73     0   NaN      NaN
    ## 2 week_2     1  4.08    NA   4.08    4.08     0  4.08  4.08     0   NaN      NaN
    ## 3 week_3     1  5.4     NA   5.4     5.4      0  5.4   5.4      0   NaN      NaN
    ## 4 week_4     1  6.41    NA   6.41    6.41     0  6.41  6.41     0   NaN      NaN
    ## 5 week_5     1  4.87    NA   4.87    4.87     0  4.87  4.87     0   NaN      NaN
    ## 6 week_6     1  6.09    NA   6.09    6.09     0  6.09  6.09     0   NaN      NaN
    ## 7 week_7     1  7.66    NA   7.66    7.66     0  7.66  7.66     0   NaN      NaN
    ## 8 week_8     1  5.83    NA   5.83    5.83     0  5.83  5.83     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[17]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  1.18    NA   1.18    1.18     0  1.18  1.18     0   NaN      NaN
    ## 2 week_2     1  2.35    NA   2.35    2.35     0  2.35  2.35     0   NaN      NaN
    ## 3 week_3     1  1.23    NA   1.23    1.23     0  1.23  1.23     0   NaN      NaN
    ## 4 week_4     1  1.17    NA   1.17    1.17     0  1.17  1.17     0   NaN      NaN
    ## 5 week_5     1  2.02    NA   2.02    2.02     0  2.02  2.02     0   NaN      NaN
    ## 6 week_6     1  1.61    NA   1.61    1.61     0  1.61  1.61     0   NaN      NaN
    ## 7 week_7     1  3.13    NA   3.13    3.13     0  3.13  3.13     0   NaN      NaN
    ## 8 week_8     1  4.88    NA   4.88    4.88     0  4.88  4.88     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[18]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  1.37    NA   1.37    1.37     0  1.37  1.37     0   NaN      NaN
    ## 2 week_2     1  1.43    NA   1.43    1.43     0  1.43  1.43     0   NaN      NaN
    ## 3 week_3     1  1.84    NA   1.84    1.84     0  1.84  1.84     0   NaN      NaN
    ## 4 week_4     1  3.6     NA   3.6     3.6      0  3.6   3.6      0   NaN      NaN
    ## 5 week_5     1  3.8     NA   3.8     3.8      0  3.8   3.8      0   NaN      NaN
    ## 6 week_6     1  4.72    NA   4.72    4.72     0  4.72  4.72     0   NaN      NaN
    ## 7 week_7     1  4.68    NA   4.68    4.68     0  4.68  4.68     0   NaN      NaN
    ## 8 week_8     1  5.7     NA   5.7     5.7      0  5.7   5.7      0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[19]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1 -0.4     NA  -0.4    -0.4      0 -0.4  -0.4      0   NaN      NaN
    ## 2 week_2     1  1.08    NA   1.08    1.08     0  1.08  1.08     0   NaN      NaN
    ## 3 week_3     1  2.66    NA   2.66    2.66     0  2.66  2.66     0   NaN      NaN
    ## 4 week_4     1  2.7     NA   2.7     2.7      0  2.7   2.7      0   NaN      NaN
    ## 5 week_5     1  2.8     NA   2.8     2.8      0  2.8   2.8      0   NaN      NaN
    ## 6 week_6     1  2.64    NA   2.64    2.64     0  2.64  2.64     0   NaN      NaN
    ## 7 week_7     1  3.51    NA   3.51    3.51     0  3.51  3.51     0   NaN      NaN
    ## 8 week_8     1  3.27    NA   3.27    3.27     0  3.27  3.27     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>
    ## 
    ## [[20]]
    ## # A tibble: 8 × 13
    ##   column     n  mean    sd median trimmed   mad   min   max range  skew kurtosis
    ##   <chr>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 week_1     1  1.09    NA   1.09    1.09     0  1.09  1.09     0   NaN      NaN
    ## 2 week_2     1  2.8     NA   2.8     2.8      0  2.8   2.8      0   NaN      NaN
    ## 3 week_3     1  2.8     NA   2.8     2.8      0  2.8   2.8      0   NaN      NaN
    ## 4 week_4     1  4.3     NA   4.3     4.3      0  4.3   4.3      0   NaN      NaN
    ## 5 week_5     1  2.25    NA   2.25    2.25     0  2.25  2.25     0   NaN      NaN
    ## 6 week_6     1  6.57    NA   6.57    6.57     0  6.57  6.57     0   NaN      NaN
    ## 7 week_7     1  6.09    NA   6.09    6.09     0  6.09  6.09     0   NaN      NaN
    ## 8 week_8     1  4.64    NA   4.64    4.64     0  4.64  4.64     0   NaN      NaN
    ## # … with 1 more variable: se <dbl>

\#\#Problem 3

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))

fill_in_missing = function(vector){
  if (is.numeric){
    
    .....
    
  }
  if (is.character){
    
    ...
  }
}
```
