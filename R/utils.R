which_ <- function(x) {
  result <- which(x)
  if(length(result) < 1) {
    NA
  } else {
    result
  }
}

strip_i <- function(x){
  gsub("\u2139", "i", x)
}
