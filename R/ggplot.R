get_dp_from_ggplotp <- function(x){
  result <- as.character(x)
  if(result[1] != "%>%") {
    get_dp_from_ggplotp(x[[2]])
  } else {
    as.list(x)[[2]]
  }
}

get_ggp_from_ggplotp <- function(x, acc = list()) {
  result <- as.character(x)
  if(result[1] == "+") {
    get_ggp_from_ggplotp(x[[2]], acc = c(acc, x[[3]]))
  } else {
    c(acc, x[[3]]) %>% rev()
  }
}

#' @importFrom utils capture.output
get_ggp_text <- function(x) {
  get_ggp_from_ggplotp(x) %>%
    map(capture.output) %>%
    paste(collapse = " +\n  ")
}

is_ggplot_pipeline <- function(x) {
  call_string <- as.character(x)
  call_string <- paste(call_string[2], call_string[1], call_string[3])
  map_lgl(c("%>%", "ggplot"), ~grepl(.x, call_string)) %>% all()
}
