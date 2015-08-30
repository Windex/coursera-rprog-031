question1 <- function() {
    set.seed(1)
    rpois(5, 2)
}

question5 <- function() {
    set.seed(10)
    x <- rep(0:1, each = 5)
    e <- rnorm(10, 0, 20)
    y <- 0.5 + 2 * x + e
    plot(x, y)
}

question8 <- function(n = 10000) {
    y <- rnorm(n)
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    library(datasets)
    Rprof()
    fit <- lm(y ~ x1 + x2)
    Rprof(NULL)
    summaryRprof()
}