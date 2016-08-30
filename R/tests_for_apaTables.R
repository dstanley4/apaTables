
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


cor_test1 <- function() {
     a <- datasets::attitude
     x <- apa.cor.table(a)
     r_apa <- x$table.body[3,4] # should be string .83**
     r_CI  <- x$table.body[4,4] # should be string [.66, .91]

     apa_m  <- x$table.body[1,2]
     apa_sd <- x$table.body[1,3]

     output    <- list()
     output$r  <- r_apa
     output$CI <- r_CI
     output$m <-  apa_m
     output$sd <- apa_sd


     return(output)
}

reg_test1 <- function() {
     d <- apaTables::album
     albumSales3 <- lm (sales ~ adverts + airplay + attract, data = d)
     beta_value <- convert_b_to_beta(summary(albumSales3)$coefficients[2,1],sd(d$adverts),sd(d$sales))
     beta_value <- round(beta_value, 7)
     return(beta_value)
}


