which_ <- function(x) {
  result <- which(x)
  if(length(result) < 1) {
    NA
  } else {
    result
  }
}

gsub_ <- function(x, pattern, replacement, ...){
  gsub(pattern = pattern, replacement = replacement, x = x, ...)
}

strip_i <- function(x){
  gsub_(x, "\u2139", "i") %>%
    gsub_("\u2018", "'") %>%
    gsub_("\u2019", "'")
}

la <- function(x, y){
  c(y, x)
}

right_pad_string <- function(x, len){
  paste0(x, paste0(rep(" ", len - nchar(x)), collapse = ""))
}

csl <- function(x){
  if(length(x) < 2){
    x
  } else {
    paste(x, collapse = ", ")
  }
}

# from Hmisc::escapeRegex
er <- function(x) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}

is_conn <- function(x){
  all(c("file", "connection") %in% class(x))
}

is_pipeline <- function(x) {
  length(as.character(x)) > 0 && as.character(x)[1] == "%>%"
}

is_assignment_pipeline <- function(x) {
  length(as.character(x)) > 0 &&
    (as.character(x)[1] == "<-" || as.character(x)[1] == "=") &&
    length(as.list(x)) > 2 &&
    is_pipeline(as.list(x)[[3]])
}
