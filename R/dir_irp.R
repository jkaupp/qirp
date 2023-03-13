#' Cross platform file path constructor for IRP Shared Drive
#'
#' @param ... character vectors of director or file paths.
#' @param fsep the path separator to use (assumed to be ASCII).
#'
#' @export
irp_path <- function(..., fsep = .Platform$file.sep) {

  if (.Platform$OS.type != "unix") {

    base_path <- file.path(.Platform$file.sep, "wfs.queensu.ca")

  } else {

    base_path <- file.path(.Platform$file.sep, "Volumes", "PBO")
  }

<<<<<<< HEAD
  file.path(base_path, list(...), fsep)
=======
  file.path(base_path, list(...), fsep = fsep)
>>>>>>> bed3738 (Added irp_path function)


}
