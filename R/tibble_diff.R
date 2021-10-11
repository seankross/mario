x <- trees
y <- trees %>%
  slice(sample(1:31, size = 15)) #%>%
  # mutate(Tall = Height > 70) %>%
  # select(Tall, Girth, Volume) %>%
  # mutate(ID = 1)

tibble_diff <- function(x, y) {
  x <- as_tibble(x)
  y <- as_tibble(y)
  result <- list()

  # select

  result$Col_Names_Position <- tibble(Column_Names = unique(c(colnames(x), colnames(y)))) %>%
    #mutate(Name_In_X = Column_Names %in% colnames(x)) %>%
    #mutate(Name_In_Y = Column_Names %in% colnames(y)) %>%
    mutate(Name_Position_X = map_dbl(Column_Names, ~ which_(.x == colnames(x)))) %>%
    mutate(Name_Position_Y = map_dbl(Column_Names, ~ which_(.x == colnames(y))))

  # arrange, slice

  x_row_index <- x %>% mutate(Mario_Row_Number_Index_X = row_number())
  y_row_index <- y %>% mutate(Mario_Row_Number_Index_Y = row_number())

  result$Row_Position <- semi_join(x, y) %>%
    right_join(x_row_index) %>%
    right_join(y_row_index) %>%
    select(Mario_Row_Number_Index_X, Mario_Row_Number_Index_Y) %>%
    rename(Index_X = Mario_Row_Number_Index_X, Index_Y = Mario_Row_Number_Index_Y)

  result$Row_Position <- result$Row_Position %>%
    bind_rows(
      tibble(Index_X = setdiff(1:nrow(x), result$Row_Position$Index_X),
             Index_Y = NA)
    ) %>%
    arrange(Index_X)


}

which_ <- function(x) {
  result <- which(x)
  if(length(result) < 1) {
    NA
  } else {
    result
  }
}
