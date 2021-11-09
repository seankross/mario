library(dplyr)

by_cyl <- mtcars %>%
  select(mpg, cyl) %>%
  group_by(cyl) %>%
  slice(c(3, 4)) %>%
  ungroup() %>%
  group_by(cyl) %>%
  mutate(avg_mpg = mean(mpg))

# # A tibble: 6 × 3
# # Groups:   cyl [3]
# mpg   cyl avg_mpg
# <dbl> <dbl>   <dbl>
# 1  22.8     4    27.6
# 2  32.4     4    27.6
# 3  21.4     6    19.8
# 4  18.1     6    19.8
# 5  16.4     8    16.8
# 6  17.3     8    16.8

by_cyl %>%
  filter(mpg < avg_mpg)

# # A tibble: 3 × 3
# # Groups:   cyl [3]
# mpg   cyl avg_mpg
# <dbl> <dbl>   <dbl>
# 1  22.8     4    27.6
# 2  18.1     6    19.8
# 3  16.4     8    16.8
