Lab 09
================
CP
2022-10-26

Create a nxk matrix of poisson variables

\##Problem 2.

``` r
set.seed(1235)
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}
f1 <- fun1(1000, 4)
mean(f1)
```

    ## [1] 4.03725

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  # YOUR CODE HERE
  x <- matrix(rpois(n*k, lambda), ncol = 4) 
  return(x)
}

f1 <- fun1alt(50000, 4)
```

``` r
#benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Unit: microseconds
    ##       expr     min      lq      mean  median       uq      max neval
    ##     fun1() 255.301 325.951 363.37498 352.701 391.2510  572.801   100
    ##  fun1alt()  11.601  13.551  33.65896  14.451  16.4005 1784.601   100

### what is a matrix and what can we do with them

``` r
d <- matrix(1:16, ncol = 4)
d
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14
    ## [3,]    3    7   11   15
    ## [4,]    4    8   12   16

## Problem 3.

find the column max function max.col()

``` r
diag(d)
```

    ## [1]  1  6 11 16

``` r
d[c(1,6,11,16)]
```

    ## [1]  1  6 11 16

``` r
d[cbind(1:4, 1:4)]
```

    ## [1]  1  6 11 16

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
M <- matrix(runif(12), ncol=4)
M
```

    ##           [,1]      [,2]        [,3]      [,4]
    ## [1,] 0.1137034 0.6233794 0.009495756 0.5142511
    ## [2,] 0.6222994 0.8609154 0.232550506 0.6935913
    ## [3,] 0.6092747 0.6403106 0.666083758 0.5449748

``` r
# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2(M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
fun2alt <- function(x) {
  # YOUR CODE HERE
 idx <- max.col( t(x))
 x[cbind(idx, 1:4)]
}
fun2alt(M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
# Benchmarking
x <- matrix(rnorm(1e4), nrow=10)
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr     min       lq     mean    median        uq      max neval
    ##     fun2(x) 885.401 987.1015 1196.314 1034.0005 1152.8510 4484.001   100
    ##  fun2alt(x)  84.701 105.4015  168.044  144.1015  172.5515 2336.002   100
