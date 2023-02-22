check_and_make_dir <- function(root, folder) {

  if (!dir.exists(file.path(root, folder))) {

    invisible(dir.create(file.path(root, folder)))

    cat(crayon::yellow(paste("Created", folder, "Folder\n", sep = " ")))

  } else {

    cat(crayon::yellow(paste(folder, "Folder Already Exists\n", sep = " ")))

  }

}



#' Function to populate a clean project directory with R, Data, Documentation, Submission
#' and Correspondence folders.
#'
#' Please Note: Unless you run this function in the root project directory, you'll have to supply the path.
#'
#' @param root_dir defaults to here::here() the project root dir
#'
#' @export
#'
#'@examples
#' # In a project directory
#' create_irp_dirs()
create_irp_dirs <- function (root_dir = here::here())
{

  root <- root_dir

  if (!dir.exists(root)) {
    stop("Root Directory Doesn't Exist")
  }

  folders <- c("R", "Data", "Documentation", "Submission", "Correspondence")

  for (i in folders) {

    check_and_make_dir(root = root, folder = i)
  }



}
