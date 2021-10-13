#' Get the argument names and values from each step of the pipeline.
#'
#' @param verbs A [base::list()] of verbs from [get_verbs()].
#' @importFrom purrr map
#' @export
get_args <- function(verbs){
  result <- verbs %>%
    map(~ if(is.name(.x) || length(as.list(.x)) <= 1) {
      list(Arg = NA, Value = NA)
    } else {
      arg_list <- as.list(.x)[-1]
      arg <- names(arg_list)
      value <- unname(arg_list)
      if(is.null(arg)){
        arg <- rep(NA, length(value))
      }
      list(Arg = arg, Value = value)
    })

  args_ <- map(result, ~ .x[["Arg"]]) %>% map(as.character)
  values <- map(result, ~ .x[["Value"]])
  list(Args = args_, Values = values)
}
