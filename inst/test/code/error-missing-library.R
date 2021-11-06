library(ggplot3)

trees %>%
  ggplot(aes(Girth, Height)) +
  geom_point(aes(color = Volume))
