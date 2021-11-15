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

``` r
df_one = read.csv("homicide-data.csv")
head(df_one)
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
tail(df_one)
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
str(df_one)
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
view(df_one)
```

This data set consists of 52179 observations and 12 variables. Of those
variables, all are characters except for “lat” and “lon”, which are
integers.

``` r
df_one = janitor::clean_names(df_one)
df_one_mutate = df_one %>% mutate(city_state = paste(city, state))
str(df_one_mutate)
```

    ## 'data.frame':    52179 obs. of  13 variables:
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
    ##  $ city_state   : chr  "Albuquerque NM" "Albuquerque NM" "Albuquerque NM" "Albuquerque NM" ...

``` r
df_one_mutate_expt = df_one_mutate %>% group_by(city_state) %>% mutate (n_homicide = n()) %>% mutate (n_unsolved = sum(disposition == "Open/No arrest", disposition == "Closed by arrest"))
view (df_one_mutate_expt)
```

I will now try prop.test

``` r
df_one_balt = df_one_mutate_expt %>% filter(city_state == "Baltimore MD")
```
