#' See the data at each step of the pipeline
#'
#' @param call A pipeline call, likely from either [parse_pipeline()]
#' or [rlang::parse_expr()].
#' @param envir An [base::environment()] where the pipeline will be evaluated.
#'
#' @importFrom tibble as_tibble is_tibble
#' @importFrom purrr map_if
#' @importFrom dplyr is_grouped_df
#' @export
get_data_steps <- function(call, envir = parent.frame()) {
  walk_pipeline(call, envir = envir) %>%
    purrr::map_if(~ is.data.frame(.x) && !dplyr::is_grouped_df(.x) && !is_tibble(.x), tibble::as_tibble)
}

walk_pipeline <- function(pipeline, acc = list(), envir = parent.frame()){
  if(is.call(pipeline)){
    acc <- c(acc, list(eval(pipeline, envir = envir)))
    pipeline <- as.list(pipeline)[[2]]
    walk_pipeline(pipeline, acc, envir)
  } else if(is.name(pipeline)){
    c(acc, list(eval(pipeline, envir = envir))) %>% rev()
  } else if(!is.language(pipeline)){
    acc %>% rev()
  } else {
    NA
  }
}
