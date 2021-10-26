#' Turn a file that ends with a pipeline into JSON
#'
#' @param path The path to an R code file.
#' @importFrom rlang parse_exprs
#' @export
file_to_json <- function(path) {
  con <- file(path, open = "r")
  on.exit(close(con))
  exprs_ <- parse_exprs(con)
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
create_jsons <- function(code_files = NULL, clobber = FALSE){
  dest_path <- file.path("inst", "test", "correct")
  if(clobber && is.null(code_files)){
    code_files <- list.files(file.path("inst", "test", "code"), full.names = TRUE)
  } else if(!is.null(code_files)) {
    code_files <- file.path("inst", "test", "code", code_files)
  } else {
    code_files <- list.files(file.path("inst", "test", "code"), full.names = TRUE) %>%
      basename() %>%
      tools::file_path_sans_ext()
    json_files <- list.files(file.path("inst", "test", "correct"), full.names = TRUE) %>%
      basename() %>%
      tools::file_path_sans_ext()
    code_files <- setdiff(code_files, json_files)
    stopifnot(length(code_files) > 0)
    code_files <- file.path("inst", "test", "code", code_files) %>%
      paste0(".R")
  }

  file.path(dest_path, basename(code_files)) %>%
    tools::file_path_sans_ext() %>%
    paste0(".json") %>%
    map2(code_files, ~writeLines(file_to_json(.y), .x))
  invisible()
}

#' @importFrom purrr walk2
check_jsons <- function(){
  code_files <- list.files(file.path("inst", "test", "code"), full.names = TRUE)
  json_files <- list.files(file.path("inst", "test", "correct"), full.names = TRUE)
  walk2(code_files, json_files, function(code, json){
    temp_file <- tempfile()
    file_to_json(code) %>% writeLines(temp_file)
    pass <- all.equal(readLines(temp_file), readLines(json))
    status <- ifelse(isTRUE(pass), "Passed", "FAILED")
    test_name <- basename(code) %>% tools::file_path_sans_ext()
    message(status, ": ", test_name)
  })
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
  } else if(Name_Strings == "rename"){
    handle_rename(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
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
# Formaldehyde %>% mutate(Two = 2, Last = Two + carb + 100) %>% parse_pipeline() -> call
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
    list(from = which(before_columns %in% value), to = which(arg == after_columns))
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

  simple_mapping <- c(from_old_columns, from_inline_columns)
  from_mapping <- list()
  to_mapping <- list()

  for (i in seq_along(simple_mapping)) {
    from <- simple_mapping[[i]][["from"]] %>% as.character()
    to <- simple_mapping[[i]][["to"]] %>% as.character()

    if (is.null(from_mapping[[from]])) {
      from_mapping[[from]] <- to
    } else {
      from_mapping[[from]] <- c(from_mapping[[from]], to)
    }

    if (is.null(to_mapping[[to]])) {
      to_mapping[[to]] <- from
    } else {
      to_mapping[[to]] <- c(to_mapping[[to]], from)
    }
  }

  from_mapping <- map2(names(from_mapping),
                       unname(from_mapping),
                       function(lhs, rhs){
                         lhs <- lhs %>% as.numeric()
                         rhs <- rhs %>% as.numeric()
                         list(illustrate = "outline", select = "column-lhs",
                              from = lhs, to = rhs)
                       })

  to_mapping <- map2(names(to_mapping),
                       unname(to_mapping),
                       function(rhs, lhs){
                         lhs <- lhs %>% as.numeric()
                         rhs <- rhs %>% as.numeric()
                         list(illustrate = "outline", select = "column-rhs",
                              from = lhs, to = rhs)
                       })

  to_mapping_endpoints <- to_mapping %>% map_dbl(~ .x$to)
  for (i in seq_along(from_values_columns)) {
    x <- from_values_columns[[i]]
    if(x$to %in% to_mapping_endpoints){
      index <- which(x$to == to_mapping_endpoints)
      to_mapping[[index]][["from_arg"]] <- x$from
    } else {
      to_mapping <- c(to_mapping, list(list(illustrate = "outline",
                                       select = "column-rhs",
                                       from_arg = x$from, to = x$to)))
    }
  }

  result[["mapping"]] <- c(from_mapping, to_mapping)#, from_values_columns)

  result
}

#' @importFrom purrr map2_lgl
handle_rename  <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- list(type = "rename")
  before_columns <- colnames(BA[[1]])
  after_columns <- colnames(BA[[2]])
  changed <- map2_lgl(before_columns, after_columns, ~ .x != .y) %>% which()

  if(any(before_columns %in% Args)){
    no_op <- which(before_columns %in% Args)
    if(!(no_op %in% changed)){
      changed <- c(changed, no_op)
    }
  }

  result[["mapping"]] <- changed %>%
    sort() %>%
    map(~ list(illustrate = "outline", select = "column", from = .x, to = .x))
  result
}
