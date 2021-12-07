library(dplyr)
library(ggplot2)

mtcars %>%
  filter(mpg > 15) %>%
  ggplot(aes(mpg, hp, color = as.factor(cyl))) +
    geom_point() +
    theme_minimal()
