#' Get the verbs from a pipeline
#'
#' @param call A pipeline call, likely from either [parse_pipeline()]
#' or [rlang::parse_expr()].
#' @returns A [base::list()] of [base::name]s or [base::call()]s.
#' @export
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   slice(1:2) %>%
#'   parse_pipeline() %>%
#'   get_verbs()
#'
#' #>
get_verbs <- function(call) {
  get_verbs_helper(call)
}

get_verbs_helper <- function(call, acc = list()) {
  if (!is_pipeline(call)) {
    c(call, acc)
  } else {
    get_verbs_helper(call[[2]], acc = c(call[[3]], acc))
  }
}
