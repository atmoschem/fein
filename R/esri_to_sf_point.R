#' get features
#'
#'@description get features.
#'
#'
#' @param features Character
#' @importFrom sf st_point st_sfc
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' }
esri_to_sf_point <- function(features) {
  get_point_geometry <- function(feature) {
    return(sf::st_point(unlist(feature$geometry)))
  }
  geoms <- sf::st_sfc(lapply(features, get_point_geometry))
  return(geoms)
}
