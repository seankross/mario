list.files("inst/test/code", full.names = TRUE) %>%
  discard(~ grepl("error", .x)) %>%
  map(function(x){
    con <- file(x, open = "r")
    on.exit(close(con))
    exprs_ <- parse_exprs(con)
    exprs_[[length(exprs_)]] %>% get_verbs() %>% get_args() %>% .$Values
  })



con <- file(path, open = "r")
on.exit(close(con))

exprs_ <- parse_exprs(code)
map(exprs_[-length(exprs_)], eval, envir = global_env())
pipeline_call <- exprs_[[length(exprs_)]]
