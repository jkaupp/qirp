#' Set Database UID and PWD in .Renviron file
#'
#' @param UID User ID to Set
#' @param PWD Password to Set
#'
#' @export
set_db_credentials <- function(UID = NA, PWD = NA) {

  current <- Sys.getenv(c("UID", "PWD"))

  cat(crayon::yellow(paste("Current UID =", ifelse(current["UID"] == "", "Blank", current["UID"]),  "\nCurrent PWD =", ifelse(current["PWD"] == "", "Blank", current["PWD"]), "\n", sep = " ")))

  choice <- utils::menu(c("Yes", "No"), title = paste("\nDo You Want To Set UID =", UID, "and PWD =", PWD))

  if (choice == 1) {

    Sys.setenv(UID = UID)
    Sys.setenv(PWD = PWD)

    cat(crayon::green("UID and PWD Updated"))

  } else {

    cat(crayon::blue("UID and PWD Unchanged"))

  }

}
