#' See the data at each step of the pipeline
#'
#' @param verbs A [base::list()] of verbs from [get_verbs()].
#' @param envir An [base::environment()] where the pipeline will be evaluated.
#'
#' @importFrom tibble as_tibble
#' @importFrom purrr accumulate map_if
#' @importFrom dplyr is_grouped_df
#' @export
get_data_steps <- function(verbs, envir = parent.frame()) {
  c(
    eval(verbs[[1]], envir = envir) %>%
      tibble::as_tibble() %>%
      list(),
    suppressMessages(purrr::accumulate(verbs, ~ call("%>%", .x, .y) %>% eval(envir = envir))[-1])
  ) %>%
    purrr::map_if(~ (is.data.frame(.x) || is.vector(.x)) && !dplyr::is_grouped_df(.x), tibble::as_tibble)
}

