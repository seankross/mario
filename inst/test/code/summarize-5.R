library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  filter(cyl < 8) %>%
  select(mpg, hp, cyl, am) %>%
  arrange(cyl, am) %>%
  slice(1, 2, 5, 12, 17, 18)

mt %>%
  summarize(Avg_MPG = mean(mpg), Avg_HP = mean(hp))
