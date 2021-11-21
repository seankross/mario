#' Get lots of information about a pipeline as a data frame.
#'
#' @param pipeline A data pipeline.
#' @importFrom tibble tibble
#' @importFrom dplyr select
#' @export
parse_pipeline_tbl <- function(pipeline) {
  pipeline %>%
    parse_pipeline() %>%
    pipeline_tbl()
}

#' Get lots of information about a pipeline as a data frame.
#'
#' @param pipeline_call A [base::call()] representing a pipeline.
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate
#' @export
pipeline_tbl <- function(pipeline_call) {
  result <- tibble(
    Verbs = pipeline_call %>% get_verbs(),
    #DF = pipeline_call %>% get_data_steps(),
    Verb_Strings = Verbs %>% as.character(),
    Args = (Verbs %>% get_args())[["Args"]],
    Values = (Verbs %>% get_args())[["Values"]],
    Names = Verbs %>% get_names(),
    Name_Strings = Names %>% as.character()
  )

  result %>%
    mutate(DF = pipeline_call %>%
             get_data_steps(envir = global_env()) %>%
             right_pad_list(nrow(result))) %>%
    select(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, everything())
}

right_pad_list <- function(x, len){
  if(length(x) >= len){
    x
  } else {
    c(x, rep(NA, len - length(x)))
  }
}
