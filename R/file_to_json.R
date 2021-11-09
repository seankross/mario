#' Turn a file that ends with a pipeline into JSON
#'
#' @param path The path to an R code file.
#' @importFrom rlang parse_exprs global_env
#' @importFrom purrr map safely
#' @export
file_to_json <- function(path) {
  con <- file(path, open = "r")
  on.exit(close(con))
  string_to_json(con)
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

#' Turn a string of a pipeline into a JSON
#'
#' @param code A string of R code.
#' @importFrom rlang parse_exprs
#' @importFrom rlang cnd_header cnd_body cnd_footer
#' @importFrom crayon strip_style
#' @export
string_to_json <- function(code) {
  result <- safely(string_to_json_helper)(code)

  if(!is.null(result[["error"]])){
    list(list(
      type = "error",
      code_step = "NA",
      mapping = list(
        message = list(header = cnd_header(result[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       body = cnd_body(result[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       footer = cnd_footer(result[["error"]]) %>%
                         strip_style() %>% strip_i())
      ),
      data_frame = "NA"
    )) %>% toJSON(auto_unbox = TRUE, pretty = TRUE)
  } else {
    result[["result"]]
  }
}

string_to_json_helper <- function(code) {
  exprs_ <- parse_exprs(code)
  map(exprs_[-length(exprs_)], eval, envir = global_env())
  pipeline_call <- exprs_[[length(exprs_)]]

  pipeline_to_json(pipeline_call)
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

#' @importFrom purrr walk2 safely
check_jsons <- function(){
  code_files <- list.files(file.path("inst", "test", "code"), full.names = TRUE)
  json_files <- file.path("inst", "test", "correct",
                          basename(code_files) %>%
                            tools::file_path_sans_ext() %>%
                            paste0(".json"))

  test_results = map2(code_files, json_files, safely(function(code, json){
    temp_file <- tempfile()
    file_to_json(code) %>% writeLines(temp_file)
    pass <- all.equal(readLines(temp_file), readLines(json))
    status <- ifelse(isTRUE(pass), "Passed", "FAILED")
    list(status = status)
  }))

  walk2(code_files, test_results, function(code, tr){
    test_name <- basename(code) %>% tools::file_path_sans_ext()

    if(!is.null(tr$error)){
      message("ERROR : ", test_name)
    } else {
      message(tr$result$status, ": ", test_name)
    }
  })
}


#' @importFrom purrr map2
before_after_tbl_list <- function(tbl_list) {
  stopifnot(length(tbl_list) > 1)
  map2(tbl_list[-length(tbl_list)], tbl_list[-1], ~ list(.x, .y))
}

#' @importFrom purrr pmap safely
pipeline_tbl_to_list <- function(ptbl) {
  pmap(ptbl, handle)
  #result <- pmap(ptbl, safely(handle))
}

handle <- function(Name_Strings, Verb_Strings, DF, Verbs,
                                    Names, Args, Values, BA){
  result <- handle_pipeline_tbl_row(Name_Strings, Verb_Strings, DF, Verbs,
                                    Names, Args, Values, BA)
  if(length(BA) > 1 && is.data.frame(BA[[1]]) && is.data.frame(BA[[2]])){
    result[["data_frame"]] <- list(
      lhs = list(col_names = I(colnames(BA[[1]])),
                 data = BA[[1]]),
      rhs = list(col_names = I(colnames(BA[[2]])),
                 data = BA[[2]])
      )
  }
  result[["code_step"]] <- Verb_Strings
  result[c("type", "code_step", "mapping", "data_frame")]
}

handle_pipeline_tbl_row <- function(Name_Strings, Verb_Strings, DF, Verbs,
                                    Names, Args, Values, BA){
  if("mario-error" %in% class(DF)) {
    handle_error(DF, BA)
  } else if(Name_Strings == "slice"){
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
  } else if(Name_Strings == "group_by"){
    handle_group_by(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else if(Name_Strings == "ungroup"){
    handle_ungroup(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  } else {
    handle_unknown(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  }
}

#' @importFrom dplyr slice
#' @importFrom purrr flatten
#' @importFrom rlang global_env
load_vars <- function(call, where = 2){
  ptbl <- mario::pipeline_tbl(call)
  ptbl$BA <- c(NA, before_after_tbl_list(ptbl$DF))
  map2(colnames(ptbl), ptbl %>% slice(where) %>% purrr::flatten(), ~assign(.x, .y, envir = global_env()))
}

result_setup <- function(type, BA, mapping = list()){
  result <- list(type = type, mapping = mapping)
  if(is_grouped_df(BA[[1]])){
    result[["mapping"]] <- c(result[["mapping"]],
                             decorate_groups(BA[[1]], "lhs"))
  }
  if(is_grouped_df(BA[[2]])){
    result[["mapping"]] <- c(result[["mapping"]],
                             decorate_groups(BA[[2]], "rhs"))
  }
  result
}

prepend_mapping <- function(new_mapping, result) {
  result[["mapping"]] <- c(new_mapping, result[["mapping"]])
  result
}

handle_error <- function(DF, BA){
  result <- list(type = "error",
       mapping = list(message = DF[["message"]]))

  if(is.data.frame(BA[[1]])){
    result[["data_frame"]] <- list(
      lhs = list(col_names = I(colnames(BA[[1]])), data = BA[[1]]),
      rhs = list(data = "NA"))
  } else {
    result[["data_frame"]] <- list(
      lhs = list(data = "NA"),
      rhs = list(data = "NA"))
  }
  result
}

#' @importFrom purrr pmap
#' @importFrom rlang parse_expr
#' @importFrom dplyr pull ungroup group_by_at
handle_slice <- function(Name_Strings, Verb_Strings, DF, Verbs,
         Names, Args, Values, BA){
  result <- result_setup("slice", BA)

  BA[[1]] <- BA[[1]] %>%
    ungroup() %>%
    mutate(row_number()) %>%
    group_by_at(group_vars(BA[[1]]))

  old_row_number_index <- paste0("BA[[1]] %>% ", Verb_Strings) %>%
    parse_expr() %>%
    eval() %>%
    pull("row_number()")

  map2(old_row_number_index, seq_along(old_row_number_index),
        ~ list(illustrate = "outline", select = "row",
               from = list(anchor = "lhs", index = .x),
               to = list(anchor = "rhs", index = .y))) %>%
    prepend_mapping(result)
}

#' @importFrom purrr pmap map map_lgl
handle_arrange <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- handle_slice(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  result[["type"]] <- "arrange"
  colnames_in_call <- map_lgl(colnames(DF), ~grepl(.x, Verb_Strings)) %>% which()
  map(colnames_in_call,
       ~list(illustrate = "highlight", select = "column",
             anchor = "lhs", index = .x)) %>%
    prepend_mapping(result)
}

handle_filter <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- handle_arrange(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
  result[["type"]] <- "filter"
  result
}

#' @importFrom dplyr contains
#' @importFrom purrr discard
handle_select <- function(Name_Strings, Verb_Strings, DF, Verbs,
                          Names, Args, Values, BA){
  result <- list(type = "select")
  suppressMessages(tbl_diff <- tibble_diff(BA[[1]], BA[[2]]))
  result[["mapping"]] <- pmap(tbl_diff$Col_Names_Position %>% select(contains("Position")),
                              ~ list(illustrate = "outline",
                                     select = "column",
                                     from = list(anchor = "lhs", index = .x),
                                     to = list(anchor = "rhs", index = .y)
                                     )) %>%
    discard(~ is.na(.x$to$index))

  result
}

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
        if(from > length(before_columns)){
          from_anchor <- "rhs"
        } else {
          from_anchor <- "lhs"
        }

        list(illustrate = "outline",
             select = "column",
             from = list(anchor = from_anchor, index = from),
             to = list(anchor = "rhs", index = to))
      })
    }) %>%
    flatten()

  from_inline_columns <- intersect(unlist(values), Args) %>%
    discard(~ any(.x == before_columns)) %>%
    map(function(z){
      list(from = which(after_columns == z),
        to = which(Args[map_lgl(values, ~ z %in% .x)] == after_columns))
    }) %>%
    map(function(x){
      if(x$from > length(before_columns)){
        from_anchor <- "rhs"
      } else {
        from_anchor <- "lhs"
      }

      list(illustrate = "outline",
           select = "column",
           from = list(anchor = from_anchor, index = x$from),
           to = list(anchor = "rhs", index = x$to))
    })

  new_col_index <- setdiff(seq_along(after_columns), seq_along(before_columns))#which(after_columns %in% Args)
  from_values_columns <- map_lgl(values, ~ all(!(.x %in% after_columns))) %>%
    which() %>%
    map(function(x){
      list(illustrate = "outline",
           select = "column",
           from = list(anchor = "arg", index = x),
           to = list(anchor = "rhs", index = new_col_index[x]))
    })

  simple_mapping <- c(from_old_columns, from_inline_columns, from_values_columns)

  to_col_index <- simple_mapping %>% map_dbl(~ .x$to$index)
  raw_cols <- setdiff(new_col_index, to_col_index) %>%
    map(function(x){
      list(illustrate = "outline",
           select = "column",
           from = list(anchor = "arg", index = x - length(before_columns)),
           to = list(anchor = "rhs", index = x))
    })

  result[["mapping"]] <- c(simple_mapping, raw_cols)
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
    map(~ list(illustrate = "outline", select = "column",
               from = list(anchor = "lhs", index = .x),
               to = list(anchor = "rhs", index = .x)))
  result
}

#' @importFrom scales hue_pal
#' @importFrom dplyr group_indices group_vars left_join
decorate_groups <- function(df, anchor = "lhs"){
  group_id <- df %>% group_indices()
  color_hex_codes <- hue_pal()(max(group_id))
  color_tbl <- tibble(Color = color_hex_codes,
                      Group_Index = 1:length(color_hex_codes))
  row_tbl <- tibble(Row_Index = 1:nrow(df),
         Group_Index = group_id)
  left_join(row_tbl, color_tbl, by = "Group_Index") %>%
    pmap(~ list(illustrate = "highlight", select = "row", anchor = anchor,
                index = ..1, group_id = ..2, color = ..3))
}

handle_group_by <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- result_setup("group_by", BA)

  which(colnames(BA[[2]]) %in% (group_vars(BA[[2]]))) %>%
    map(~ list(illustrate = "outline", select = "column", anchor = "lhs",
              index = .x)) %>%
    prepend_mapping(result)
}

handle_ungroup <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- result_setup("ungroup", BA)
  values <- Values %>%
    as.character() %>%
    intersect(group_vars(BA[[1]]))

  which(colnames(BA[[1]]) %in% values) %>%
    map(~ list(illustrate = "outline", select = "column", anchor = "lhs",
               index = .x)) %>%
    prepend_mapping(result)
}

handle_unknown <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  list(type = "unknown", mapping = list())
}
