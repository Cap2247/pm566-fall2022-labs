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
# Benchmarking
#microbenchmark::microbenchmark(
#  fun1(),
 # fun1alt()
#
```

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
