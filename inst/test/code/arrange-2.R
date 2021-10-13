library(dplyr)

BOD %>%
  arrange(desc(demand), Time)
