#' get features
#'
#'@description get features.
#'
#'
#' @param ids Integer.
#' @param url Character.
#' @param query_url Character.
#' @param fields Character.
#' @param token Character.
#' @param ssl Logical, default = FALSE.
#' @param simplifyDataFrame Logical, default = FALSE.
#' @param simplifyVector Logical, default = FALSE.
#' @param full Logical, default = FALSE.
#' @importFrom jsonlite fromJSON
#' @importFrom httr content POST config
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' ids <- get_object_ids()
#' feat <- get_esri_features_by_ids(ids = ids)
#' }
get_esri_features_by_ids <- function(
  ids,
  url = paste0(
    "http://portal1.snirh.gov.br/ana/",
    "rest/services/Esta%C3%A7%C3%B5es_da_",
    "Rede_Hidrometeorol%C3%B3gica_Nacional",
    "_em_Opera%C3%A7%C3%A3o/MapServer/1"
  ),
  query_url = paste(url, "query", sep = "/"),
  fields = c("*"),
  token = '',
  ssl = FALSE,
  simplifyDataFrame = FALSE,
  simplifyVector = FALSE,
  full = FALSE
) {
  # create Simple Features from ArcGIS servers json response
  query <- list(
    objectIds = paste(ids, collapse = ","),
    outFields = paste(fields, collapse = ","),
    token = token,
    outSR = '4326',
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
  response <- jsonlite::fromJSON(
    response_raw,
    simplifyDataFrame = simplifyDataFrame,
    simplifyVector = simplifyVector,
    digits = NA
  )
  if (full) {
    return(response)
  } else {
    esriJsonFeatures <- response$features
    return(esriJsonFeatures)
  }
}
