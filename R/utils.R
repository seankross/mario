which_ <- function(x) {
  result <- which(x)
  if(length(result) < 1) {
    NA
  } else {
    result
  }
}
