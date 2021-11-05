library(dplyr)

mt <- mtcars %>%
  slice(1:5) %>%
  select(mpg, cyl, disp, hp)

mt %>%
  select(-cyl, mpg, hp, disp) %>%
  select(mpg, hp, disp)
