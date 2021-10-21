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

# # For debugging
# Formaldehyde %>% mutate(Sum = carb + optden, TripleSum = Sum * 3, Two = 2, Col = sapply(2:7, function(x){x - 1})) %>% parse_pipeline() -> call
# Formaldehyde %>% mutate(carb = carb * 2) %>% parse_pipeline() -> call
# Formaldehyde %>% mutate(Quack = sapply(2:7, function(x){x - 1})) %>% parse_pipeline() -> call
# Formaldehyde %>% mutate(DoubleCarb = carb * 2, TinyCarb = DoubleCarb / 200) %>% parse_pipeline() -> call
# Formaldehyde %>% mutate(Sum = carb + optden, carb = carb * 2) %>% parse_pipeline() -> call
# ptbl <- pipeline_tbl(call)
# ptbl$BA <- c(NA, mario:::before_after_tbl_list(ptbl$DF))
# map2(colnames(ptbl), ptbl %>% slice(2) %>% purrr::flatten(), ~assign(.x, .y, envir = globalenv()))

#' @importFrom purrr flatten discard
handle_mutate  <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- list(type = "mutate")
  before_columns <- colnames(BA[[1]])
  after_columns <- colnames(BA[[2]])
  values <- Values %>%
    as.character() %>%
    strsplit("[^\\w\\d\\._]+", perl = TRUE)
  mutated_columns <- Args %>% as.character()

  # The goal is to map from the sources of data to the new columns
  # There are three sources of data:
  # - from existing columns: mutate(Sum = carb + optden)
  # - from columns created inline in the mutate: mutate(Double = card * 2, Triple = Double * 1.5)
  # - from "raw" values: mutate(Two = 2)

  from_old_columns <- map2(Args, values, function(arg, value){
    list(from = which(value %in% before_columns), to = which(arg == after_columns))
  }) %>%
    discard(~ length(.x$from) < 1) %>%
    map(function(z){
      map2(z$from, rep(z$to, length(z$from)), function(from, to){
        list(illustrate = "outline", select = "column", from = from, to = to)
      })
    }) %>%
    flatten()

  from_inline_columns <- intersect(unlist(values), Args) %>%
    discard(~ any(.x == before_columns)) %>%
    map(function(z){
      list(from = which(after_columns == z),
        to = which(Args[map_lgl(values, ~ z %in% .x)] == after_columns))
    }) %>%
    map(~ list(illustrate = "outline", select = "column", from = .x$from, to = .x$to))

  new_col_index <- which(after_columns %in% Args)
  from_values_columns <- map_lgl(values, ~ all(!(.x %in% after_columns))) %>%
    which() %>%
    map(~list(illustrate = "outline",
              select = "code-column",
              from = .x, to = new_col_index[.x]))

  result[["mapping"]] <- c(from_old_columns,
                           from_inline_columns,
                           from_values_columns)

  result
}
