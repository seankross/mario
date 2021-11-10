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
