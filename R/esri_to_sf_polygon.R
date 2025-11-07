#' get features
#'
#'@description get features.
#'
#'
#' @param features Character
#' @importFrom sf st_multipolygon st_sfc
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' }
esri_to_sf_polygon <- function(features) {
  ring2matrix <- function(ring) {
    return(do.call(rbind, lapply(ring, unlist)))
  }
  rings2multipoly <- function(rings) {
    return(sf::st_multipolygon(list(lapply(rings, ring2matrix))))
  }
  getGeometry <- function(feature) {
    if (is.null(unlist(feature$geometry$rings))) {
      return(sf::st_multipolygon())
    } else {
      return(rings2multipoly(feature$geometry$rings))
    }
  }
  geoms <- sf::st_sfc(lapply(features, getGeometry))
  return(geoms)
}
