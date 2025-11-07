#' get features
#'
#'@description get features.
#'
#'
#' @param features Character
#' @importFrom sf st_multilinestring st_sfc
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' }
esri_to_sf_polyline <- function(features) {
  path2matrix <- function(path) {
    return(do.call(rbind, lapply(path, unlist)))
  }
  paths2multiline <- function(paths) {
    return(sf::st_multilinestring(lapply(paths, path2matrix)))
  }
  getGeometry <- function(feature) {
    return(paths2multiline(feature$geometry$paths))
  }
  geoms <- sf::st_sfc(lapply(features, getGeometry))
  return(geoms)
}
