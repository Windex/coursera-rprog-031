library(datasets)
data(iris)
data(mtcars)

question1 <- function() {
    tapply(iris$Sepal.Length, iris$Species, mean)["virginica"]
}

question2 <- function() {
    apply(iris[, 1:4], 2, mean)
}

question3 <- function() {
    tapply(mtcars$mpg, mtcars$cyl, mean)
}

question4 <- function() {
    hp <- tapply(mtcars$hp, mtcars$cyl, mean)
    abs(hp["4"] - hp["8"])
}