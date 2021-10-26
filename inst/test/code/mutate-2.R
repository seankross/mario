library(dplyr)

Formaldehyde %>%
  mutate(Sum = carb + optden,
         TripleSum = Sum * 3,
         Two = 2,
         Col = sapply(2:7, function(x){x - 1}))
