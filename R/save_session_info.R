#' Save session information
#'
#' \code{save_session_info} writes the output of
#' \code{sessioninfo::session_info()} to file.
#'
#' @param path Path of written file.
#'
#' @export
save_session_info <- function(path = "session.log") {
  options(width = 80)
  writeLines(utils::capture.output(sessioninfo::session_info()), path)
}
