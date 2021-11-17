library(dplyr)

mt <- mtcars %>%
  as_tibble() %>%
  filter(cyl < 8) %>%
  select(mpg, cyl, am) %>%
  arrange(cyl, am) %>%
  slice(1, 2, 5, 12, 17, 18) %>%
  group_by(cyl, am)

mt %>%
  summarize(mean(mpg),
         Sum = cyl + am + 100,
         am = NULL,
         cyl = 5,
         Two = 2,
         Three = 3,
         DoubleSum = Sum * 2 + Two,
         PipeSum = 1:10 %>% sum(),
         Three = NULL,
         Cool = sapply(2:3, function(x){x * 10}),
         1 + 3)
