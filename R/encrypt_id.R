#' Encrypt IDs for U15 Projects
#'
#' @param id An id or vector of ids to encrypt
#'
#' @return an encrypted id
#' @export
encrypt_id <- function(id) {

  openssl::md5(id)

}
