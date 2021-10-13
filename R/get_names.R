#' Get the data and function names from each step of the pipeline.
#'
#' @param verbs A [base::list()] of verbs from [get_verbs()].
#' @importFrom purrr map
#' @export
get_names <- function(verbs) {
  verbs %>%
    map(~ if(is.name(.x)) {.x} else {.x[[1]]})
}
