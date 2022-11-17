Assignment_4
================
CP
2022-11-16

### SQL

``` r
library(RSQLite)
```

    ## Warning: package 'RSQLite' was built under R version 4.2.2

``` r
library(DBI)
```

    ## Warning: package 'DBI' was built under R version 4.2.2

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

``` r
dbGetQuery(con, "PRAGMA table_info(category)")
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0 category_id INTEGER       0         NA  0
    ## 2   1        name    TEXT       0         NA  0
    ## 3   2 last_update    TEXT       0         NA  0

``` sql
PRAGMA table_info(film)
```

## Question 1

How many many movies is there avaliable in each rating catagory.

``` r
dbGetQuery(con,"
SELECT  rating, COUNT(*) AS count
FROM  film
GROUP BY rating
")
```

    ##   rating count
    ## 1      G   180
    ## 2  NC-17   210
    ## 3     PG   194
    ## 4  PG-13   223
    ## 5      R   195

## Question 2

What is the average replacement cost and rental rate for each rating
category.

``` r
dbGetQuery(con,"
SELECT  rating, replacement_cost, rental_rate,
COUNT(*) AS count,
AVG(replacement_cost) AS avgreplacement_cost
FROM  film
GROUP BY rating
")
```

    ##   rating replacement_cost rental_rate count avgreplacement_cost
    ## 1      G            12.99        4.99   180            20.12333
    ## 2  NC-17            18.99        2.99   210            20.13762
    ## 3     PG            20.99        0.99   194            18.95907
    ## 4  PG-13            28.99        4.99   223            20.40256
    ## 5      R            15.99        4.99   195            20.23103

## Question 3

Use table film_category together with film to find the how many films
there are with each category ID

``` r
dbGetQuery(con,"
SELECT  film.film_id, film_category.category_id,
COUNT(*) AS N
FROM film
INNER JOIN film_category  ON film.film_id = film_category.film_id
GROUP BY category_id
")
```

    ##    film_id category_id  N
    ## 1       19           1 64
    ## 2       18           2 66
    ## 3       48           3 60
    ## 4       14           4 57
    ## 5        7           5 58
    ## 6        1           6 68
    ## 7       33           7 62
    ## 8        5           8 69
    ## 9        6           9 73
    ## 10      46          10 61
    ## 11       2          11 56
    ## 12      12          12 51
    ## 13      22          13 63
    ## 14      26          14 61
    ## 15      10          15 74
    ## 16      41          16 57

## Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` r
dbGetQuery(con,"
SELECT  film.film_id, film_category.category_id,category.category_id, category.name,
COUNT(*) AS N
FROM film
INNER JOIN film_category  ON film.film_id = film_category.film_id
INNER JOIN category ON category.category_id = film_category.category_id
GROUP BY name 
")
```

    ##    film_id category_id category_id        name  N
    ## 1       19           1           1      Action 64
    ## 2       18           2           2   Animation 66
    ## 3       48           3           3    Children 60
    ## 4       14           4           4    Classics 57
    ## 5        7           5           5      Comedy 58
    ## 6        1           6           6 Documentary 68
    ## 7       33           7           7       Drama 62
    ## 8        5           8           8      Family 69
    ## 9        6           9           9     Foreign 73
    ## 10      46          10          10       Games 61
    ## 11       2          11          11      Horror 56
    ## 12      12          12          12       Music 51
    ## 13      22          13          13         New 63
    ## 14      26          14          14      Sci-Fi 61
    ## 15      10          15          15      Sports 74
    ## 16      41          16          16      Travel 57

Category 15 which is Sports is the most popular category with 74 movies.

### HPC

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}
```

``` r
fun1alt <- function(mat) {
  rowSums(mat)
}
```

``` r
# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)
```

``` r
# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat))
```

    ## Unit: microseconds
    ##          expr     min       lq      mean   median      uq      max neval
    ##     fun1(dat) 202.300 227.0505 482.77196 378.3505 556.701 4245.002   100
    ##  fun1alt(dat)  47.801  59.5510  72.79196  60.8010  62.351 1060.800   100

``` r
# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}
```

``` r
fun2alt <- function(mat) {
  cumsum(mat) }
```

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat))
```

    ## Unit: microseconds
    ##          expr      min       lq       mean    median       uq      max neval
    ##     fun2(dat) 1674.202 1899.601 2154.96812 2016.1505 2212.051 5901.001   100
    ##  fun2alt(dat)   37.901   61.551   92.99098   69.6505   87.301 1006.901   100

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    1.88    0.70    2.58

``` r
library(parallel)
```

``` r
system.time({
  cl <- parallel::makePSOCKcluster(2L)

parallel::clusterSetRNGStream(cl, 123)
  ans <- unlist(parallel::parLapply(cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  parallel::stopCluster(cl)
})
```

    ## [1] 3.141365

    ##    user  system elapsed 
    ##    0.02    0.02    2.55
