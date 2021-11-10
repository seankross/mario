library(dplyr)

Formaldehyde %>%
  mutate(Sum = carb + optden + 100,
         carb = NULL,
         optden = "A",
         Two = 2,
         Three = 3,
         DoubleSum = Sum * 2 + Two,
         PipeSum = 1:10 %>% sum(),
         Three = NULL,
         Cool = sapply(2:7, function(x){x * 10}),
         1 + 3)

#   optden     Sum Two DoubleSum PipeSum Cool 1 + 3
# 1      A 100.186   2   202.372      55   20     4
# 2      A 100.569   2   203.138      55   30     4
# 3      A 100.946   2   203.892      55   40     4
# 4      A 101.138   2   204.276      55   50     4
# 5      A 101.326   2   204.652      55   60     4
# 6      A 101.682   2   205.364      55   70     4
