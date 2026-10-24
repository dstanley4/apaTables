# TRUE when a CSS rule flextable wrote raises its run as a superscript.
# flextable 0.9.12 writes vertical-align:super; 0.9.10, which DESCRIPTION
# still allows, writes position: relative with a bottom offset (a subscript
# gets a top offset instead, so this does not match one).
css_is_superscript <- function(rule) {
  grepl("vertical-align:\\s*super", rule) ||
    grepl("position:\\s*relative;\\s*bottom:\\s*[0-9.]+pt", rule)
}

# The CSS rule flextable wrote for one class in a rendered table, "" if none.
ft_css_rule <- function(html, cl) {
  r <- regmatches(html, regexpr(paste0("\\.", cl, "\\{[^}]*\\}"), html))
  if (length(r)) r else ""
}

# Every font size set on a flextable, header and body parts together.
ft_font_sizes <- function(ft) {
  sort(unique(c(as.vector(ft$header$styles$text$font.size$data),
                as.vector(ft$body$styles$text$font.size$data))))
}

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
     r_apa <- x$table_body[5,5] # should be string .83**
     r_CI  <- x$table_body[6,5] # should be string [.66, .91]

     apa_m  <- x$table_body[1,3]
     apa_sd <- x$table_body[1,4]

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
