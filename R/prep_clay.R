#' Prep Clay for map-making.
#'
#' @param shapes Shapes to map
#' @param map_detail The level of detail for the map; min is 1 and max is 14, defaults to 6
#'
#' @return A list of data objects needed to mold 2- and 3-d clay maps
#' @export
#'
#' @examples \dontrun
prep_clay = function(shapes, map_detail = 6){

  if(sf::st_crs(shapes) != sf::st_crs(3005)){
    shapes = sf::st_transform(shapes, 3005)
  }

  cat("\nDigging up clay...\n")

  elev = terra::rast(
    suppressMessages(
      elevatr::get_elev_raster(
        locations = shapes,
        z = map_detail)
    )
  )

  mbase = make_map_base(shapes, buffer = 0.1)

  elev_mb = terra::crop(terra::mask(elev, mbase), mbase)

  cat("\nKneading clay...\n")

  dist_to_border = suppressWarnings(suppressMessages(terra::distance(elev_mb, ws_borders)))

  cat("\nClay prepped!\n")

  return(
    list(
      shapes = shapes,
      mbase = mbase,
      map_detail = map_detail,
      elev = elev,
      elev_mb = elev_mb,
      dist_to_border = dist_to_border
    )
  )
}
