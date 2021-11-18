library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  filter(cyl < 8) %>%
  select(mpg, cyl, am) %>%
  arrange(cyl, am) %>%
  slice(1, 2, 5, 12, 17, 18) %>%
  group_by(cyl, am)

mt %>%
  summarize(Min_MPG = min(mpg), Max_MPG = max(mpg))
