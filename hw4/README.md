Assignment 04 - HPC and SQL
================

## Due Date

~~October 28~~ November 4th, 2020 by midnight Pacific time.

The learning objectives are to conduct data scraping and perform text
mining.

## HPC

### Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

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
fun1alt <- function(mat) {
  rowSums(mat)
}
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
fun2alt <- function(mat) {
  t(apply(dat,1,cumsum))
}
# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)
# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq       max neval
    ##     fun1(dat) 4.717026 7.087311 5.387063 6.913684 6.815113 0.7440926   100
    ##  fun1alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.0000000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr     min       lq     mean   median       uq       max neval
    ##     fun2(dat) 4.50167 4.425539 3.499337 4.148637 3.579889 0.9547098   100
    ##  fun2alt(dat) 1.00000 1.000000 1.000000 1.000000 1.000000 1.0000000   100

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

### Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

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

In order to get accurate estimates, we can run this function multiple
times, with the following code:

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
    ##    3.13    0.00    3.13

Rewrite the previous code using `parLapply()` to make it run faster.
Make sure you set the seed using `clusterSetRNGStream()`:

``` r
library(parallel)

system.time({
  
 cl=makePSOCKcluster(4)
 clusterSetRNGStream(cl, 1231)
  ans <- unlist(parLapply(cl,1:4000, sim_pi, n = 10000))
  print(mean(ans))
  stopCluster(cl)
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##    0.00    0.03    1.73

## SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))
library(RSQLite)
```

    ## Warning: package 'RSQLite' was built under R version 4.0.3

``` r
library(DBI)
```

    ## Warning: package 'DBI' was built under R version 4.0.3

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

When you write a new chunk, remember to replace the `r` with `sql,
connection=con`. Some of these questions will reqruire you to use an
inner join. Read more about them here
<https://www.w3schools.com/sql/sql_join_inner.asp>

## Question 1

How many many movies is there avaliable in each `rating` catagory.

``` sql
select rating, count(film_id) as movies
from film
group by rating
```

<div class="knitsql-table">

| rating | movies |
| :----- | -----: |
| G      |    180 |
| NC-17  |    210 |
| PG     |    194 |
| PG-13  |    223 |
| R      |    195 |

5 records

</div>

## Question 2

What is the average replacement cost and rental rate for each `rating`
category.

``` sql
select rating,avg(replacement_cost) as average_replacement_cost , avg(rental_rate) as average_rental_rate
from  film 
group by rating
```

<div class="knitsql-table">

| rating | average\_replacement\_cost | average\_rental\_rate |
| :----- | -------------------------: | --------------------: |
| G      |                   20.12333 |              2.912222 |
| NC-17  |                   20.13762 |              2.970952 |
| PG     |                   18.95907 |              3.051856 |
| PG-13  |                   20.40256 |              3.034843 |
| R      |                   20.23103 |              2.938718 |

5 records

</div>

## Question 3

Use table `film_category` together with `film` to find the how many
films there are with each category ID

``` sql
select category_id , count(a.film_id) as films
from film_category a join film b
on a.film_id=b.film_id
group by category_id
```

<div class="knitsql-table">

| category\_id | films |
| :----------- | ----: |
| 1            |    64 |
| 2            |    66 |
| 3            |    60 |
| 4            |    57 |
| 5            |    58 |
| 6            |    68 |
| 7            |    62 |
| 8            |    69 |
| 9            |    73 |
| 10           |    61 |

Displaying records 1 - 10

</div>

## Question 4

Incorporate table `category` into the answer to the previous question to
find the name of the most popular category.

``` sql
select name,a.category_id , count(a.film_id) as films
from film_category a join film b on a.film_id=b.film_id
join category c on a.category_id=c.category_id 
group by a.category_id
order by films desc
```

<div class="knitsql-table">

| name        | category\_id | films |
| :---------- | -----------: | ----: |
| Sports      |           15 |    74 |
| Foreign     |            9 |    73 |
| Family      |            8 |    69 |
| Documentary |            6 |    68 |
| Animation   |            2 |    66 |
| Action      |            1 |    64 |
| New         |           13 |    63 |
| Drama       |            7 |    62 |
| Sci-Fi      |           14 |    61 |
| Games       |           10 |    61 |

Displaying records 1 - 10

</div>

Here we can see that the most popular film category is sports(id:15)
with 74 films.

``` r
# clean up
dbDisconnect(con)
```
