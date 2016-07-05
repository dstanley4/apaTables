
d_value_test1 <- function() {
     set.seed(1)
     x <- rnorm(10)+1
     y <- rnorm(10)
     d <- get_d_value(x,y)
     return(d)
}

d_value_test2 <- function() {
     set.seed(1)
     x <- rnorm(10)+1
     y <- rnorm(10)
     d <- get_d_value(y,x) #reverse order
     return(d)
}
