#' get features
#'
#'@description get features.
#'
#'
#' @param json_feats Character
#' @param geom_type Character.
#' @param crs CRS.
#' @importFrom sf st_sf
#' @importFrom data.table rbindlist setDF
#' @importFrom magrittr "%>%"
#' @export
#' @seealso \code{\link{layer_info}}
#' @examples \dontrun{
#' # do not run
#' }
esri_to_sf_geom <- function(json_feats, geom_type, crs = 4326) {
  # convert esri json to simple feature
  if (geom_type == 'esriGeometryPolygon') {
    geoms <- esri_to_sf_polygon(json_feats)
  }
  if (geom_type == 'esriGeometryPoint') {
    geoms <- esri_to_sf_point(json_feats)
  }
  if (geom_type == 'esriGeometryPolyline') {
    geoms <- esri_to_sf_polyline(json_feats)
  }
  # attributes
  atts <- lapply(json_feats, '[[', 1) %>%
    lapply(function(att) {
      lapply(att, function(x) return(ifelse(is.null(x), NA, x)))
    })

  af <- data.table::rbindlist(lapply(
    atts,
    as.data.frame.list,
    stringsAsFactors = FALSE
  ))
  data.table::setDF(af)
  # geometry + attributes
  df <- sf::st_sf(geoms, af, crs = crs)
  return(df)
}
