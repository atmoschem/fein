#' get layer info
#'
#'@description get layer info.
#'
#'
#' @param url Character, default .
#' @param token Character.
#' @param ssl Logical, default = FALSE.
#' @importFrom jsonlite fromJSON
#' @importFrom httr content POST config
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' a <- layer_info()
#' names(a)
#' a$geometryType
#' }
layer_info <- function(
  url = paste0(
    "http://portal1.snirh.gov.br/ana/",
    "rest/services/Esta%C3%A7%C3%B5es_da_",
    "Rede_Hidrometeorol%C3%B3gica_Nacional",
    "_em_Opera%C3%A7%C3%A3o/MapServer/1"
  ),
  token = '',
  ssl = FALSE
) {
  jsonlite::fromJSON(
    httr::content(
      httr::POST(
        url,
        query = list(f = "json", token = token),
        encode = "form",
        config = httr::config(ssl_verifypeer = ssl)
      ),
      as = "text"
    )
  )
}
