#' Turn a file that ends with a pipeline into JSON
#'
#' @param path The path to an R code file.
#' @importFrom rlang parse_exprs global_env
#' @importFrom purrr map safely
#' @importFrom readr read_file
#' @export
file_to_json <- function(path) {
  code <- read_file(path)
  string_to_json(code)
}

#' Turn a pipeline call into JSON
#'
#' @param call A pipeline call, likely from either [parse_pipeline()]
#' or [rlang::parse_expr()].
#' @importFrom jsonlite toJSON
#' @importFrom pryr object_size
#' @export
pipeline_to_json <- function(call){
  ptbl <- pipeline_tbl(call)
  ptbl$BA <- c(NA, before_after_tbl_list(ptbl$DF))

  (pipeline_tbl_to_list(ptbl)[-1]) %>% toJSON(auto_unbox = TRUE, pretty = TRUE)
}

#' @importFrom jsonlite toJSON
pipeline_to_trace <- function(call){
  ptbl <- pipeline_tbl(call)

  if(as.numeric(object_size(ptbl)) > 1000000){
    error_message <- paste("Your total data is",
                           as.numeric(object_size(ptbl)),
                           "bytes, which is over the maximum of 1MB",
                           "that this tool currently supports.")

    stop(error_message, call. = FALSE)
  }

  ptbl$BA <- c(NA, before_after_tbl_list(ptbl$DF))

  (pipeline_tbl_to_list(ptbl)[-1])
}

#' Turn a string of a pipeline into a JSON
#'
#' @param code A string of R code.
#' @importFrom rlang parse_exprs
#' @importFrom rlang cnd_header cnd_body cnd_footer
#' @importFrom crayon strip_style

#' @export
string_to_json <- function(code) {
  trace_raw <- safely(string_to_json_helper)(code)

  if(!is.null(trace_raw[["error"]])){
    trace_ <- list(list(
      type = "error",
      code_step = "NA",
      mapping = list(
        message = list(header = cnd_header(trace_raw[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       body = cnd_body(trace_raw[["error"]]) %>%
                         strip_style() %>% strip_i(),
                       footer = cnd_footer(trace_raw[["error"]]) %>%
                         strip_style() %>% strip_i())
      ),
      data_frame = "NA"
    ))
  } else {
    trace_ <- trace_raw[["result"]]
  }

  result <- list(
    code = code,
    trace = trace_
  )

  result %>% toJSON(auto_unbox = TRUE, pretty = TRUE)
}

string_to_json_helper <- function(code) {
  exprs_ <- parse_exprs(code)
  map(exprs_[-length(exprs_)], eval, envir = global_env())

  if(length(exprs_) < 1){
    stop("No code detected.", call. = FALSE)
  }

  pipeline_call <- exprs_[[length(exprs_)]]

  if(is_pipeline(pipeline_call)){
    pipeline_to_trace(pipeline_call)
  } else if(is_assignment_pipeline(pipeline_call)){
    pipeline_to_trace(as.list(pipeline_call)[[3]])
  } else {
    stop("No data pipeline detected.", call. = FALSE)
  }
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
#' @importFrom jsonlite fromJSON
check_jsons <- function(){
  code_files <- list.files(file.path("inst", "test", "code"), full.names = TRUE)
  json_files <- file.path("inst", "test", "correct",
                          basename(code_files) %>%
                            tools::file_path_sans_ext() %>%
                            paste0(".json"))

  longest_name <- basename(code_files) %>%
    tools::file_path_sans_ext() %>%
    nchar() %>%
    max()

  suppressMessages({
  test_results = map2(code_files, json_files, safely(function(code, json){
    if(!file.exists(json)){ return(list(status = "NONE  ")) }
    temp_file <- tempfile()
    file_to_json(code) %>% writeLines(temp_file)
    pass <- all.equal(readLines(temp_file), readLines(json))
    status <- ifelse(isTRUE(pass), "Passed", "FAILED")
    list(status = status, type = csl(fromJSON(json)[["trace"]][["type"]]))
  }))
  })

  walk2(code_files, test_results, function(code, tr){
    test_name <- basename(code) %>% tools::file_path_sans_ext()

    if(!is.null(tr$error)){
      message("ERROR : ", test_name)
    } else {
      message(tr$result$status, ": ", right_pad_string(test_name, longest_name),
              " | ", tr$result$type)
    }
  })

  summary_ <- paste0("Passed: ", test_results %>% map_lgl(~ .x$result$status == "Passed") %>% sum(),
          " | FAILED: ", test_results %>% map_lgl(~ .x$result$status == "FAILED") %>% sum(),
          " | ERROR: ", test_results %>% map_lgl(~ !is.null(.x$result$error)) %>% sum(),
          " | NONE: ", test_results %>% map_lgl(~ .x$result$status == "NONE  ") %>% sum())
  message(paste0(rep("-", nchar(summary_)), collapse = ""))
  message(summary_)
}

#' @importFrom purrr map2
before_after_tbl_list <- function(tbl_list) {
  stopifnot(length(tbl_list) > 1)
  map2(tbl_list[-length(tbl_list)], tbl_list[-1], ~ list(.x, .y))
}

#' @importFrom purrr pmap safely
pipeline_tbl_to_list <- function(ptbl) {
  pmap(ptbl, handle)
}

#' @importFrom dplyr group_keys group_split
handle <- function(Name_Strings, Verb_Strings, DF, Verbs,
                                    Names, Args, Values, BA){
  result <- handle_pipeline_tbl_row(Name_Strings, Verb_Strings, DF, Verbs,
                                    Names, Args, Values, BA)

  if(length(BA) > 0 &&
     is.data.frame(BA[[1]]) &&
     is.null(result[["data_frame"]][["lhs"]])){
    result[["data_frame"]] <- list(
      lhs = list(col_names = I(colnames(BA[[1]])),
                 data = BA[[1]])) %>% la(result[["data_frame"]])
  }

  if(length(BA) > 1 &&
     is.data.frame(BA[[2]]) &&
     is.null(result[["data_frame"]][["rhs"]])){
    result[["data_frame"]] <- list(
      rhs = list(col_names = I(colnames(BA[[2]])),
                 data = BA[[2]])) %>% la(result[["data_frame"]])
  }

  if(length(BA) > 0 && is_grouped_df(BA[[1]])){
    result[["data_frame"]][["lhs"]][["group_data"]] <- list(
      col_names = BA[[1]] %>% group_keys() %>% colnames() %>% I(),
      group_indices = BA[[1]] %>% group_indices(),
      group_keys = BA[[1]] %>% group_keys() %>%
        group_by_at(group_vars(BA[[1]])) %>%
        group_split() %>% map2(BA[[1]] %>%
                                 group_keys() %>%
                                 group_by_at(group_vars(BA[[1]])) %>%
                                 group_indices(),
                               ~list(group_id = .y, key = .x %>% as.list()))
    )
  }

  if(length(BA) > 1 && is_grouped_df(BA[[2]])){
    result[["data_frame"]][["rhs"]][["group_data"]] <- list(
      col_names = BA[[2]] %>% group_keys() %>% colnames() %>% I(),
      group_indices = BA[[2]] %>% group_indices(),
      group_keys = BA[[2]] %>% group_keys() %>%
        group_by_at(group_vars(BA[[2]])) %>%
        group_split() %>% map2(BA[[2]] %>%
                                 group_keys() %>%
                                 group_by_at(group_vars(BA[[2]])) %>%
                                 group_indices(),
                               ~list(group_id = .y, key = .x %>% as.list()))
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
  } else if(Name_Strings %in% c("summarize", "summarise")){
    handle_summarize(Name_Strings, Verb_Strings, DF, Verbs, Names, Args, Values, BA)
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
  if(length(BA) > 0 && is_grouped_df(BA[[1]])){
    result[["mapping"]] <- c(result[["mapping"]],
                             decorate_groups(BA[[1]], "lhs"))
  }
  if(length(BA) > 1 && is_grouped_df(BA[[2]])){
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
  result <- result_setup("select", BA)
  cn1 <- colnames(BA[[1]])
  cn2 <- colnames(BA[[2]])

  # Did any names get changed?
  if(any(nzchar(Args))){
    names(Values) <- Args
    new_names <- Args %>% discard(Negate(nzchar))
    for(i in new_names){
      # Assign old name to same position as new name
      cn2[which(cn2 == i)] <- Values[[i]] %>% as.character()
    }
  }

  map2(map_dbl(cn2, ~ which(.x == cn1)), seq_along(cn2),
                              ~ list(illustrate = "outline",
                                     select = "column",
                                     from = list(anchor = "lhs", index = .x),
                                     to = list(anchor = "rhs", index = .y)
                                     )) %>%
    discard(~ is.na(.x$to$index)) %>%
    prepend_mapping(result)
}

#' @importFrom purrr flatten discard map_chr
#' @importFrom dplyr select_at
#' @importFrom stringr str_extract_all
handle_mutate  <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- result_setup("mutate", BA)
  before_columns <- colnames(BA[[1]])
  after_columns <- colnames(BA[[2]])
  new_columns <- setdiff(after_columns, before_columns)
  value_s <- Values %>% as.character()

  quoted_values <- Values %>%
    as.character() %>%
    map(function(x){
      str_extract_all(x, "`.+`") %>% unlist() %>% gsub_("`", "")
    }) %>%
    map(~ifelse(length(.x) > 0, .x, NA))

  values <- Values %>%
    map2(quoted_values, function(v, qv){
      if(is.null(v)){
        return(v)
      } else if(is.na(qv)){
        v_ <- v %>% as.character()
      } else {
        v_ <- v %>% as.character() %>% map_chr(~ gsub(er(qv), "", .x))
      }

      v_ <- v_ %>%
        strsplit("[^\\w\\d\\._]+", perl = TRUE) %>%
        unlist() %>%
        discard(Negate(nzchar))

      c(v_, qv) %>% discard(~ is.na(.x))
    })

  # The goal is to map from the sources of data to the new columns
  # There are three sources of data:
  # - from existing columns: mutate(Sum = carb + optden)
  # - from columns created inline in the mutate: mutate(Double = card * 2, Triple = Double * 1.5)
  # - from "raw" values: mutate(Two = 2)

  pmap(list(Args, values, seq_along(Args), value_s), function(a, v, i, vs){
    #i = 2; a = Args[i]; v = values[[i]]; vs = value_s[i]
    mapping <- list()
    visited <- FALSE
    # is it null? (we do not draw columns created and nulled inline)
    if(is.null(v) && (a %in% before_columns)) {
      mapping <- list(
        illustrate = "crossout",
        select = "column",
        from = list(
          anchor = "arg",
          index = i
        ),
        to = list(
          anchor = "lhs",
          index = which(a == before_columns)
        )
      ) %>% list()
      return(mapping)
    }

    # is it from an old column?
    if(any(v %in% before_columns)) {
      visited <- TRUE

      if (a == "" && (vs %in% after_columns)) {
        to_index <- which(vs == after_columns)
      } else {
        to_index <- which(a == after_columns)
      }

      mapping <- intersect(v, before_columns) %>%
        map(~list(
          illustrate = "outline",
          select = "column",
          from = list(
            anchor = "lhs",
            index = which(.x == before_columns)
          ),
          to = list(
            anchor = "rhs",
            index = to_index
          )
        )) %>% la(mapping)
    }

    # is it from a new column?
    if(any(v %in% new_columns) && !(a == "" && (vs %in% after_columns) && !any(v %in% Args) && !any(v %in% value_s[-i]))) {
      visited <- TRUE

      if (a == "" && (vs %in% after_columns)) {
        to_index <- which(vs == after_columns)
      } else {
        to_index <- which(a == after_columns)
      }

      mapping <- intersect(v, new_columns) %>%
        map(~list(
          illustrate = "outline",
          select = "column",
          from = list(
            anchor = "rhs",
            index = which(.x == after_columns)
          ),
          to = list(
            anchor = "rhs",
            index = to_index
          )
        )) %>% la(mapping)
    }

    # Assume it's from a value
    if(!visited && ((a %in% after_columns) || vs %in% after_columns)) {
      to_index <- ifelse(a %in% after_columns,
                         which(a == after_columns),
                         which(vs == after_columns))

      mapping <- list(
        illustrate = "outline",
        select = "column",
        from = list(
          anchor = "arg",
          index = i
        ),
        to = list(
          anchor = "rhs",
          index = to_index
        )
      ) %>% la(mapping) %>% list()
    }

    if(length(mapping) < 1) {
      mapping <- list(illustrate = "skip") %>% list()
    }

    mapping
  }) %>%
    flatten() %>%
    discard(~ .x[["illustrate"]] == "skip") %>%
    prepend_mapping(result)
}

#' @importFrom purrr map2_lgl
handle_rename  <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result <- result_setup("rename", BA)
  before_columns <- colnames(BA[[1]])
  after_columns <- colnames(BA[[2]])
  changed <- map2_lgl(before_columns, after_columns, ~ .x != .y) %>% which()

  if(any(before_columns %in% Args)){
    no_op <- which(before_columns %in% Args)
    if(!(no_op %in% changed)){
      changed <- c(changed, no_op)
    }
  }

  changed %>%
    sort() %>%
    map(~ list(illustrate = "outline", select = "column",
               from = list(anchor = "lhs", index = .x),
               to = list(anchor = "rhs", index = .x))) %>%
    prepend_mapping(result)
}

#' @importFrom dplyr group_indices group_vars left_join
decorate_groups <- function(df, anchor = "lhs"){
  group_id <- df %>% group_indices()
  row_tbl <- tibble(Row_Index = 1:nrow(df),
         Group_Index = group_id)
  row_tbl %>%
    pmap(~ list(illustrate = "highlight", select = "row", anchor = anchor,
                index = ..1, group_id = ..2))
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

handle_summarize <- function(Name_Strings, Verb_Strings, DF, Verbs,
                             Names, Args, Values, BA){
  result <- result_setup("summarize", BA)
  before_columns <- colnames(BA[[1]])
  after_columns <- colnames(BA[[2]])
  new_columns <- setdiff(after_columns, before_columns)
  value_s <- Values %>% as.character()
  gvb <- group_vars(BA[[1]])
  gva <- group_vars(BA[[2]])

  quoted_values <- Values %>%
    as.character() %>%
    map(function(x){
      str_extract_all(x, "`.+`") %>% unlist() %>% gsub_("`", "")
    }) %>%
    map(~ifelse(length(.x) > 0, .x, NA))

  values <- Values %>%
    map2(quoted_values, function(v, qv){
      if(is.null(v)){
        return(v)
      } else if(is.na(qv)){
        v_ <- v %>% as.character()
      } else {
        v_ <- v %>% as.character() %>% map_chr(~ gsub(er(qv), "", .x))
      }

      v_ <- v_ %>%
        strsplit("[^\\w\\d\\._]+", perl = TRUE) %>%
        unlist() %>%
        discard(Negate(nzchar))

      c(v_, qv) %>% discard(~ is.na(.x))
    })

  after_group <- BA[[2]] %>% group_by_at(gvb) %>% group_indices()
  after_row <- BA[[2]] %>% ungroup() %>% mutate(row_number()) %>% pull("row_number()")
  to_rows <- BA[[1]] %>%
    group_indices() %>%
    map(~ after_row[which(.x == after_group)])
  from_rows <- 1:nrow(BA[[1]])

  result <- pmap(list(Args, values, seq_along(Args), value_s), function(a, v, i, vs){
    #i = 1; a = Args[i]; v = values[[i]]; vs = value_s[i]
    mapping <- list()
    visited <- FALSE

    # Named args are not group vars &&
    # Values are not group vars &&
    # Some values are in before columns
    if(!(a %in% gvb) && !any(v %in% gvb) && any(v %in% before_columns)){
      visited <- TRUE

      if(a != "") {
        to_index <- which(a == after_columns)
      } else {
        to_index <- which(vs == after_columns)
      }

      mapping <- intersect(v, before_columns) %>%
        map(function(col){
          map2(from_rows, to_rows, function(from, to){
            map(to, ~list(illustrate = "outline",
                          select = "cell",
                          from = list(
                            anchor = "lhs",
                            index = c(from, which(col == before_columns))
                          ),
                          to = list(
                            anchor = "rhs",
                            index = c(.x, to_index)
                          )
            ))
          })
        }) %>% flatten() %>% flatten() %>%  la(mapping)
    }

    # Assume it's from a value
    if(!visited && ((a %in% after_columns) || vs %in% after_columns)) {
      to_index <- ifelse(a %in% after_columns,
                         which(a == after_columns),
                         which(vs == after_columns))

      mapping <- list(
        illustrate = "outline",
        select = "column",
        from = list(
          anchor = "arg",
          index = i
        ),
        to = list(
          anchor = "rhs",
          index = to_index
        )
      ) %>% la(mapping) %>% list()
    }

    if(length(mapping) < 1) {
      mapping <- list(illustrate = "skip") %>% list()
    }

    mapping
  }) %>%
    flatten() %>%
    discard(~ .x[["illustrate"]] == "skip") %>%
    prepend_mapping(result)

  result <- map(gvb, function(group){
    if(!(group %in% Args) && !(group %in% unlist(values))){
      intersect(group, before_columns) %>%
        map(function(col){
          map2(from_rows, to_rows, function(from, to){
            map(to, ~list(illustrate = "outline",
                          select = "cell",
                          from = list(
                            anchor = "lhs",
                            index = c(from, which(col == before_columns))
                          ),
                          to = list(
                            anchor = "rhs",
                            index = c(.x, which(col == after_columns))
                          )
            ))
          })
        })
    }
  }) %>% flatten() %>% flatten() %>% flatten() %>%
    prepend_mapping(result)

  result
}

handle_unknown <- function(Name_Strings, Verb_Strings, DF, Verbs,
                           Names, Args, Values, BA){
  result_setup("unknown", BA)
}
