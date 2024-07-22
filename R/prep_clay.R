#' Prep Clay for map-making.
#'
#' @param shapes Either a single {sf} polygon object, or a list thereof, to map
#' @param map_detail The level of detail for the map; min is 1 and max is 14, defaults to 6
#'
#' @return A list of data objects needed to mold 2-d and 3-d clay maps
#' @export
#'
#' @examples \dontrun
prep_clay = function(shapes, map_detail = 6){

  # Ensure all shapes are in BC Albers (3005)
  shapes = shapes |>
    lapply(\(x) {
      if(sf::st_crs(x) != sf::st_crs(3005)){
        x = sf::st_transform(x, 3005)
      }
      x
    })

  # Find the total extent of all shapes.
  max_ext = find_shapes_max_ext(shapes)

  # Convert total extent to a square shape.
  max_ext_sf = data.frame(x = c(max_ext$xmin, max_ext$xmax),
                          y = c(max_ext$ymin, max_ext$ymax)) |>
    sf::st_as_sf(coords = c('x','y'), crs = 3005) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_as_sf()

  # Combine shapes.
  shapes_c = shapes |>
    lapply(summarise) |>
    dplyr::bind_rows() |>
    dplyr::summarise()

  cat("\nDigging up clay...\n")

  elev = terra::rast(
    suppressMessages(
      elevatr::get_elev_raster(
        locations = shapes_c,
        z = map_detail)
    )
  )

  # Crop elevation to extent of shapes

  mbase = make_map_base(shapes_c, buffer = 0.05)

  elev_mb = terra::crop(terra::mask(elev, mbase), mbase)

  elev_c = terra::crop(terra::mask(elev, shapes_c), shapes_c)

  cat("\nKneading clay...\n")

  dist_r = terra::crop(elev, mbase)
  dist_r[] <- 0

  # Something weird about this distance to border piece...
  # dist_to_border = suppressWarnings(suppressMessages(terra::distance(elev_mb,shapes_c)))
  dist_to_border = suppressWarnings(
    suppressMessages(
      terra::distance(
        dist_r,
        terra::vect(shapes_c)
      )
    )
  )

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
