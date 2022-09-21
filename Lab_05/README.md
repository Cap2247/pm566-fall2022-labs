Lab 05
================
CP
2022-09-21

## Read in the data

First download and then read in with data.table:fread()

``` r
if (!file.exists("../lab03/met_all.gz"))
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
```

``` r
met <- data.table::fread("met_all.gz")
```

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(dtplyr)
```

Read in the stations data

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

Merge met data with stations

``` r
merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 
```

    ##          USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ##       1: 690150 93121 2019     8   1    0  56 34.300 -116.166  696      220
    ##       2: 690150 93121 2019     8   1    1  56 34.300 -116.166  696      230
    ##       3: 690150 93121 2019     8   1    2  56 34.300 -116.166  696      230
    ##       4: 690150 93121 2019     8   1    3  56 34.300 -116.166  696      210
    ##       5: 690150 93121 2019     8   1    4  56 34.300 -116.166  696      120
    ##      ---                                                                   
    ## 2377339: 726813 94195 2019     8  31   19  56 43.650 -116.633  741       70
    ## 2377340: 726813 94195 2019     8  31   20  56 43.650 -116.633  741       NA
    ## 2377341: 726813 94195 2019     8  31   21  56 43.650 -116.633  741       10
    ## 2377342: 726813 94195 2019     8  31   22  56 43.642 -116.636  741       10
    ## 2377343: 726813 94195 2019     8  31   23  56 43.642 -116.636  741       40
    ##          wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##       1:           5              N     5.7          5      22000             5
    ##       2:           5              N     8.2          5      22000             5
    ##       3:           5              N     6.7          5      22000             5
    ##       4:           5              N     5.1          5      22000             5
    ##       5:           5              N     2.1          5      22000             5
    ##      ---                                                                       
    ## 2377339:           5              N     2.1          5      22000             5
    ## 2377340:           9              C     0.0          5      22000             5
    ## 2377341:           5              N     2.6          5      22000             5
    ## 2377342:           1              N     2.1          1      22000             1
    ## 2377343:           1              N     2.1          1      22000             1
    ##          ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc
    ##       1:                 9        N    16093           5       N          5
    ##       2:                 9        N    16093           5       N          5
    ##       3:                 9        N    16093           5       N          5
    ##       4:                 9        N    16093           5       N          5
    ##       5:                 9        N    16093           5       N          5
    ##      ---                                                                   
    ## 2377339:                 9        N    16093           5       N          5
    ## 2377340:                 9        N    16093           5       N          5
    ## 2377341:                 9        N    14484           5       N          5
    ## 2377342:                 9        N    16093           1       9          9
    ## 2377343:                 9        N    16093           1       9          9
    ##          temp temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh
    ##       1: 37.2       5      10.6            5    1009.9            5 19.88127
    ##       2: 35.6       5      10.6            5    1010.3            5 21.76098
    ##       3: 34.4       5       7.2            5    1010.6            5 18.48212
    ##       4: 33.3       5       5.0            5    1011.6            5 16.88862
    ##       5: 32.8       5       5.0            5    1012.7            5 17.38410
    ##      ---                                                                    
    ## 2377339: 32.2       5      12.2            5    1012.8            5 29.40686
    ## 2377340: 33.3       5      12.2            5    1011.6            5 27.60422
    ## 2377341: 35.0       5       9.4            5    1010.8            5 20.76325
    ## 2377342: 34.4       1       9.4            1    1010.1            1 21.48631
    ## 2377343: 34.4       1       9.4            1    1009.6            1 21.48631
    ##          CTRY STATE
    ##       1:   US    CA
    ##       2:   US    CA
    ##       3:   US    CA
    ##       4:   US    CA
    ##       5:   US    CA
    ##      ---           
    ## 2377339:   US    ID
    ## 2377340:   US    ID
    ## 2377341:   US    ID
    ## 2377342:   US    ID
    ## 2377343:   US    ID

``` r
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

Remove temperatures less than -17C and change elev 9999 to missing value

``` r
met <- met[temp > -17][elev ==9999.0, elev:=NA]
```

Question 1.

Compute mean temperature, wind speed and

``` r
station_averages <- met[,. (temp= mean(temp, na.rm=T),
                            wind.sp = mean(wind.sp, na.rm=T),
                            atm.press= mean(atm.press, na.rm=T)), by = USAFID]
```

The above computes the mean by weather station.

\##Question 2 Now lets compute the median value for each variable

``` r
stmeds <- station_averages[,. (
                            temp50     = median(temp, na.rm=T),
                            windsp50   = median(wind.sp, na.rm=T),
                            atmpress50 = median(atm.press, na.rm=T)
)]
```

``` r
station_averages[, temp_dist50 := abs(temp - stmeds$temp50)][order(temp_dist50)]
```

    ##       USAFID      temp   wind.sp atm.press  temp_dist50
    ##    1: 720458 23.681730  1.209682       NaN  0.002328907
    ##    2: 725515 23.686388  2.709164       NaN  0.002328907
    ##    3: 725835 23.678347  2.652381       NaN  0.005712423
    ##    4: 724509 23.675100  4.066833  1013.863  0.008959632
    ##    5: 720538 23.665932  1.907897       NaN  0.018127186
    ##   ---                                                  
    ## 1584: 722788 36.852459  3.393852       NaN 13.168399783
    ## 1585: 722787 37.258907  2.847381       NaN 13.574848130
    ## 1586: 723805 37.625391  3.532935  1005.207 13.941331392
    ## 1587: 726130  9.189602 12.239908       NaN 14.494456787
    ## 1588: 720385  8.044959  7.298963       NaN 15.639100105

``` r
head(station_averages)
```

    ##    USAFID     temp  wind.sp atm.press temp_dist50
    ## 1: 690150 33.18763 3.483560  1010.379   9.5035752
    ## 2: 720110 31.22003 2.138348       NaN   7.5359677
    ## 3: 720113 23.29317 2.470298       NaN   0.3908894
    ## 4: 720120 27.01922 2.503079       NaN   3.3351568
    ## 5: 720137 21.88823 1.979335       NaN   1.7958292
    ## 6: 720151 27.57686 2.998428       NaN   3.8928051

lets use ‘which.min()’ It summarizes the min from above

``` r
summary(met$elev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   -13.0   101.0   252.0   414.3   400.0  4113.0     182

Generate a date variable

``` r
met <- met[, ymd := as.Date(paste(year, month, day, sep = "-"))]
```

Using the data.table::week function, keep the observations of the first
week of the month.

``` r
met[, table(week(ymd))]
```

    ## 
    ##     31     32     33     34     35 
    ## 297259 521600 527922 523847 446576

``` r
met <-met[week(ymd)==31]
```

Compute the mean by station of the variables temp, rh, wind.sp,
vis.dist, dew.point, lat, lon, and elev.

``` r
met[,.(
  temp     = max(temp,na.rm=TRUE),
  rh       = max(rh,na.rm=TRUE),
  wind.sp  = max(wind.sp,na.rm=TRUE),
  vis.dist = max(vis.dist,na.rm=TRUE),
  lat      = max(lat ,na.rm=TRUE),
  lon      = max(lon ,na.rm=TRUE), 
  elev     = max(elev,na.rm=TRUE)
)]
```

    ##    temp  rh wind.sp vis.dist    lat     lon elev
    ## 1:   47 100    20.6   144841 48.941 -68.313 4113

Great! No more 9999s in our dataset.

``` r
met_avg <- met[,.(
  temp     = mean(temp,na.rm=TRUE),
  rh       = mean(rh,na.rm=TRUE),
  wind.sp  = mean(wind.sp,na.rm=TRUE),
  vis.dist = mean(vis.dist,na.rm=TRUE),
  dew.point= mean(dew.point, na.rm=TRUE),
  lat      = mean(lat),
  lon      = mean(lon), 
  elev     = mean(elev,na.rm=TRUE)
), by=c("USAFID")]
```

Create a region variable for NW, SW, NE, SE based on lon = -93. 8.00 and
lat = 39.71 degrees

``` r
met_avg[, region := 
                fifelse(lon >=-98 & lat > 39.71, "NE",
                fifelse(lon <-98 & lat > 39.71, "NW",
                fifelse(lon <-98 & lat <= 39.71, "SW","SE")))]

 table(met_avg$region)                     
```

    ## 
    ##  NE  NW  SW 
    ## 484 146 945

Create a categorical variable for elevation as in the lecture slides

``` r
met_avg[, elev_cat := fifelse(elev > 252, "high", "low")]
```

## 3. MAke Violin plots of dew point temp by region
