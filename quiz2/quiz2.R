question1 <- function() {
    cube <- function(x, n) {
        x ^ 3
    }
    
    cube(3)
}

question2 <- function() {
    x <- 1:10
    if (x > 5) {
        x <- 0
    }
}

question3 <- function() {
    f <- function(x) {
        g <- function(y) {
            y + z # lexical scope, use the z from f since g is defined in f
                  # despite z being defined afterwards
        }
        z <- 4
        x + g(x)
    }
    
    z <- 10
    f(3)
}

question4 <- function() {
    x <- 5
    y <- if (x < 3) {
        NA
    } else {
        10
    }
    
    y
}