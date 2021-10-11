get_names <- function(verbs) {
  verbs %>%
    map(~ if(is.name(.x)) {.x} else {.x[[1]]})
}

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

parse_pipeline_tbl <- function(pipeline) {
  pipeline_call <- parse_pipeline(pipeline)
  tibble(
    Verbs = pipeline_call %>% get_verbs(),
    DF = Verbs %>% get_data_steps(),
    Verb_Strings = Verbs %>% as.character(),
    Args = (Verbs %>% get_args())[["Args"]],
    Values = (Verbs %>% get_args())[["Values"]],
    Names = Verbs %>% get_names(),
    Name_Strings = Names %>% as.character()
  ) %>%
    select(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, everything())
}

pipeline_call <- mtcars %>%
  select(mpg, cyl, disp) %>%
  mutate(mpg2 = mpg * 2, cyl = cyl / 4) %>%
  head() %>%
  parse_pipeline()

