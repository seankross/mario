#' Turn a file that ends with a pipeline into JSON
#'
#' @param path The path to an R code file.
#' @importFrom rlang parse_exprs
#' @export
file_to_json <- function(path) {
  exprs_ <- parse_exprs(file(path))
  on.exit(close(file(path)))
  pipeline_call <- exprs_[[length(exprs_)]]

  pipeline_to_json(pipeline_call)
}

#' Turn a pipeline call into JSON
#'
#' @param call A pipeline call, likely from either [parse_pipeline()]
#' or [rlang::parse_expr()].
#' @importFrom jsonlite toJSON
#' @export
pipeline_to_json <- function(call){
  ptbl <- pipeline_tbl(call)
  ptbl$BA <- c(NA, before_after_tbl_list(ptbl$DF))

  (pipeline_tbl_to_list(ptbl)[-1]) %>% toJSON(auto_unbox = TRUE, pretty = TRUE)
}

# When you have the mario project open in RStudio, run this with
# mario:::create_jsons()
#' @importFrom purrr map2
create_jsons <- function(r_files = list.files(system.file("test", "code", package = "mario"), full.names = TRUE),
                         dest_path = file.path("inst", "test", "correct")){
  file.path(dest_path, basename(r_files)) %>%
    tools::file_path_sans_ext() %>%
    paste0(".json") %>%
    map2(r_files, ~writeLines(file_to_json(.y), .x))
  invisible()
}


#' @importFrom purrr map2
before_after_tbl_list <- function(tbl_list) {
  stopifnot(length(tbl_list) > 1)
  map2(tbl_list[-length(tbl_list)], tbl_list[-1], ~ list(.x, .y))
}

#' @importFrom purrr pmap
pipeline_tbl_to_list <- function(ptbl) {
  pmap(ptbl, handle_pipeline_tbl_row)
}

handle_pipeline_tbl_row <- function(Name_Strings, Verb_Strings, DF, Verbs,
                                    Names, Args, Values, BA){
  if(Name_Strings == "slice"){
    handle_slice(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else if(Name_Strings == "arrange"){
    handle_arrange(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else if(Name_Strings == "filter"){
    handle_filter(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else if(Name_Strings == "select"){
    handle_select(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else if(Name_Strings == "mutate"){
    handle_mutate(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else {
    list(type = "NA")
  }
}

#' @importFrom purrr pmap
handle_slice <- function(Name_Strings, Verb_Strings, DF, Verbs,
         Names, Args, Values, BA){
  result <- list(type = "slice")
  suppressMessages(tbl_diff <- tibble_diff(BA[[1]], BA[[2]]))
  result[["mapping"]] <- pmap(tbl_diff$Row_Position, ~ list(illustrate = "outline", select = "row", from = .x, to = .y))
  result
}

#' @importFrom purrr pmap map map_lgl
handle_arrange <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- handle_slice(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  result[["type"]] <- "arrange"
  colnames_in_call <- map_lgl(colnames(DF), ~grepl(.x, Verb_Strings)) %>% which()
  result[["mapping"]] <- map(colnames_in_call,
       ~list(illustrate = "highlight", select = "column", index = .x)) %>%
    c(result[["mapping"]])
  result
}

handle_filter <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- handle_arrange(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  result[["type"]] <- "filter"
  result
}

#' @importFrom dplyr contains
handle_select <- function(Name_Strings, Verb_Strings, DF, Verbs,
                          Names, Args, Values, BA){
  result <- list(type = "select")
  #suppressMessages(tbl_diff <- tibble_diff(ptbl$BA[[2]][[1]], ptbl$BA[[2]][[2]]))
  suppressMessages(tbl_diff <- tibble_diff(BA[[1]], BA[[2]]))
  result[["mapping"]] <- pmap(tbl_diff$Col_Names_Position %>% select(contains("Position")),
                              ~ list(illustrate = "outline", select = "column", from = .x, to = .y))
  result
}

# Formaldehyde %>% mutate(Sum = carb + optden, TripleSum = Sum * 3, Two = 2) %>% parse_pipeline() -> call
# Formaldehyde %>% mutate(carb = carb * 2) %>% parse_pipeline() -> call
# Formaldehyde %>% mutate(DoubleCarb = carb * 2, TinyCarb = DoubleCarb / 200) %>% parse_pipeline() -> call
# ptbl <- pipeline_tbl(call)
# ptbl$BA <- c(NA, mario:::before_after_tbl_list(ptbl$DF))
# map2(colnames(ptbl), ptbl %>% slice(2) %>% purrr::flatten(), ~assign(.x, .y, envir = globalenv()))

#' @importFrom purrr flatten
handle_mutate  <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- list(type = "mutate")
  before_columns <- colnames(BA[[1]])
  after_columns <- colnames(BA[[2]])
  values <- Values %>% as.character()
  mutated_columns <- Args %>% as.character()

  map2(Args, values, function(arg, value){
    after_columns %>% keep(~ grepl(.x, value))
  }) %>%
    map_if(~ length(.x) < 1, ~NA) %>%
    map2(Args, function(from, to){
      map2(rep(from, length(to)), to, ~list(from = .x, to = .y))
    }) %>%
    flatten() %>%
    map(function(x){
      if(is.na(x$from)){

      }
    })

  Args

  result[["mapping"]] <- after_columns %>%
    map(~ mutated_columns[grep(.x, source_args)]) %>%
    map2(after_columns, function(to, from){
      map2(rep(from, length(to)), to, ~list(from = .x, to = .y))
    }) %>%
    flatten() %>%
    map(~ list(illustrate = "outline", select = "column",
               from = which(.x[["from"]] == after_columns),
               to = which(.x[["to"]] == after_columns)))
  result
}
