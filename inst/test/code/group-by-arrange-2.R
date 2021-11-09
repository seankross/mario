library(dplyr)

by_cyl <- mtcars %>%
  select(mpg, cyl, wt) %>%
  group_by(cyl) %>%
  slice(c(3, 4)) %>%
  ungroup() %>%
  group_by(cyl)

# # A tibble: 6 × 3
# # Groups:   cyl [3]
# mpg   cyl    wt
# <dbl> <dbl> <dbl>
# 1  22.8     4  3.15
# 2  32.4     4  2.2
# 3  21.4     6  3.22
# 4  18.1     6  3.46
# 5  16.4     8  4.07
# 6  17.3     8  3.73

by_cyl %>%
  arrange(desc(wt), .by_group = TRUE)

# # A tibble: 6 × 3
# # Groups:   cyl [3]
# mpg   cyl    wt
# <dbl> <dbl> <dbl>
# 1  22.8     4  3.15
# 2  32.4     4  2.2
# 3  18.1     6  3.46
# 4  21.4     6  3.22
# 5  16.4     8  4.07
# 6  17.3     8  3.73
