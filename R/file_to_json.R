#' Turn a file that ends with a pipeline into JSON
#'
#' @param path The path to an R code file.
#' @importFrom rlang parse_exprs
#' @importFrom jsonlite toJSON
#' @export
file_to_json <- function(path) {
  # eval_env <- new_environment(parent = parent.frame())
  exprs_ <- parse_exprs(file(path))
  on.exit(close(file(path)))
  # setup <- exprs_[-length(exprs_)]
  pipeline <- exprs_[[length(exprs_)]]

  # map(setup, eval, envir = eval_env)
  # eval(pipeline, envir = eval_env)

  ptbl <- pipeline_tbl(pipeline)
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

handle_select <- function(Name_Strings, Verb_Strings, DF, Verbs,
                          Names, Args, Values, BA){
  result <- list(type = "select")
  suppressMessages(tbl_diff <- tibble_diff(BA[[1]], BA[[2]]))
  result[["mapping"]] <- pmap(tbl_diff$Row_Position, ~ list(illustrate = "outline", select = "row", from = .x, to = .y))
}
