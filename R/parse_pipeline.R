#' Capture a pipeline as a call
#'
#' @param pipeline A data pipeline.
#'
#' @export
parse_pipeline <- function(pipeline) {
  frame_number <- 1
  pipeline <- sys.call(frame_number)[[2]]

  while (class(pipeline) != "call") {
    frame_number <- frame_number - 1
    pipeline <- sys.call(frame_number)[[2]]
  }

  result <- sys.call(frame_number)
  lhs <- as.character(result)

  while (grepl("parse_pipeline", lhs[2])) {
    result <- result[[2]]
    lhs <- as.character(result)
  }

  result[[2]]
}
