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

#' @importFrom purrr safely
#' @importFrom rlang cnd_header cnd_body cnd_footer
#' @importFrom crayon strip_style
walk_pipeline <- function(pipeline, acc = list(), envir = parent.frame()){
  if(is.call(pipeline) && is_pipeline(pipeline)){
    result <- safely(function(x){eval(x, envir = envir)})(pipeline)
    if(!is.null(result[["error"]])){
      result <- list(
        error = TRUE,
        message = list(header = cnd_header(result[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       body = cnd_body(result[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       footer = cnd_footer(result[["error"]]) %>%
                         strip_style() %>% strip_i())
      )
      class(result) <- "mario-error"
    } else {
      result <- result$result
    }
    acc <- c(acc, list(result))
    pipeline <- as.list(pipeline)[[2]]
    walk_pipeline(pipeline, acc, envir)
  } else if(is.name(pipeline) || is.call(pipeline)){
    result <- safely(function(x){eval(x, envir = envir)})(pipeline)
    if(!is.null(result[["error"]])){
      result <- list(
        error = TRUE,
        message = list(header = cnd_header(result[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       body = cnd_body(result[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       footer = cnd_footer(result[["error"]]) %>%
                         strip_style() %>% strip_i())
      )
      class(result) <- "mario-error"
    } else {
      result <- result$result
    }
    c(acc, list(result)) %>% rev()
  } else if(!is.language(pipeline)){
    acc %>% rev()
  } else {
    NA
  }
}
