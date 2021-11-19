json <- "inst/test/correct/summarize-6.json"
#json <- "e.json"

trace_ <- fromJSON(json) %>% as.list()
mapping <- trace_$mapping[[1]] %>% as_tibble() %>% tidyr::unnest(cols = where(~length(.x) > 1),
                                                        names_repair = "minimal")
colnames(mapping) <- c("illustrate", "select", "lhs_anchor", "lhs_index",
                         "rhs_anchor", "rhs_index", "anchor", "index", "group_id")
lhs_df <- trace_$data_frame$lhs$data[[1]]
rhs_df <- trace_$data_frame$rhs$data[[1]]

index_index <- mapping %>%
  filter(select == "cell") %>%
  select(lhs_index, rhs_index) %>%
  tidyr::unnest_wider("lhs_index", names_sep = c("_row", "_col")) %>%
  tidyr::unnest_wider("rhs_index", names_sep = c("_row", "_col"))

plot(0, 0, col = "white",
  xlim = c(0, length(lhs_df) + length(rhs_df) + 1),
  ylim = c(0, max(nrow(lhs_df), nrow(rhs_df))))

for(row in 1:nrow(lhs_df)){
  for(col in 1:length(lhs_df)){
    rect(col - 1, row - 1, col, row)
    text(col - 0.5, rev(1:nrow(lhs_df))[row] - 0.5, lhs_df[[row, col]])
  }
}

for(row in 1:nrow(rhs_df)){
  for(col in 1:length(rhs_df)){
    rect(col - 1 + length(lhs_df) + 1, row - 1, col + length(lhs_df) + 1, row)
    text(col - 0.5 + length(lhs_df) + 1, rev(1:nrow(rhs_df))[row] - 0.5, rhs_df[[row, col]])
  }
}

for(i in 1:nrow(index_index)){
  lrow <- rev(1:nrow(lhs_df))[index_index$lhs_index_row1[i]] - 0.5
  lcol <- index_index$lhs_index_col2[i] - 0.25
  rrow <- rev(1:nrow(rhs_df))[index_index$rhs_index_row1[i]] - 0.5
  rcol <- index_index$rhs_index_col2[i] + length(lhs_df) + 0.25
  color <- scales::hue_pal()(length(lhs_df))[round(lcol)]
  arrows(lcol, lrow, rcol, rrow, col = color, lty = 2)
}
