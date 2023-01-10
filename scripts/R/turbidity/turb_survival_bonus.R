library(tidyverse)

# params from inSALMO
p1 <- 0.1
p2 <- 0.9
x1 <- 5
x2 <- 80

d <- log(p1 / (1 - p1))
c <- log(p2 / (1 - p2))
logistic_b <- (d - c) / (x1 - x2)
logistic_a <- d - (logistic_b * x1)

tibble(
  turb_int = logistic_a,
  turb_slope = logistic_b
)
