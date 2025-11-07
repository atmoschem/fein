#' get layer info
#'
#'@description get layer info.
#'
#'
#' @param url Character .
#' @param query_url Character.
#' @param where Character.
#' @param token Character.
#' @param ssl Logical, default = FALSE.
#' @importFrom jsonlite fromJSON
#' @importFrom httr content POST config
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' a <- get_object_ids()
#' head(a)
#' a <- get_object_ids(full = T)
#' names(a)
#' a$objectIds
#' }
get_object_ids <- function(
  url = paste0(
    "http://portal1.snirh.gov.br/ana/",
    "rest/services/Esta%C3%A7%C3%B5es_da_",
    "Rede_Hidrometeorol%C3%B3gica_Nacional",
    "_em_Opera%C3%A7%C3%A3o/MapServer/1"
  ),
  query_url = paste(url, "query", sep = "/"),
  where = "1=1",
  token = '',
  ssl = FALSE,
  full = FALSE
) {
  query <- list(
    where = where,
    returnIdsOnly = "true",
    token = token,
    f = "json"
  )
  response_raw <- httr::content(
    httr::POST(
      query_url,
      body = query,
      encode = "form",
      config = httr::config(ssl_verifypeer = ssl)
    ),
    as = "text"
  )
  response <- jsonlite::fromJSON(response_raw)
  if (full) {
    return(response)
  } else {
    return(response$objectIds)
  }
}
