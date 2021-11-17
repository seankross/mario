library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  filter(cyl < 8) %>%
  select(mpg, cyl, am) %>%
  arrange(cyl, am) %>%
  slice(1, 2, 5, 12, 17, 18) %>%
  group_by(cyl, am)

mt %>%
  summarize(cyl, Avg_MPG = mean(mpg))# %>% parse_pipeline() %>% mario:::load_vars()

# # A tibble: 6 × 3
# # Groups:   cyl, am [4]
#   mpg   cyl    am
# <dbl> <dbl> <dbl>
# 1  24.4     4     0
# 2  22.8     4     0
# 3  32.4     4     1
# 4  21.4     6     0
# 5  21       6     1
# 6  19.7     6     1
#
# # A tibble: 6 × 2
# # Groups:   cyl, am [4]
#   cyl    am
# <dbl> <dbl>
# 1     4     0
# 2     4     0
# 3     4     1
# 4     6     0
# 5     6     1
# 6     6     1
