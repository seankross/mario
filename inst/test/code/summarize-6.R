library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  filter(cyl < 8) %>%
  select(mpg, cyl, am) %>%
  arrange(cyl, am) %>%
  slice(1, 2, 5, 12, 17, 18) %>%
  group_by(cyl)


mt %>%
  summarise(qs = quantile(mpg, c(0.25, 0.75)), prob = c(0.25, 0.75))
