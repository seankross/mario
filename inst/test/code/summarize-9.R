library(dplyr)

x <- mtcars %>%
  group_by(cyl)

# summarizing on a single column with no operation should have a more simple
# pattern of arrows
x %>%
  summarize(disp)
