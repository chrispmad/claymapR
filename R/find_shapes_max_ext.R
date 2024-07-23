#' Find the maximum extent of shapes
#'
#' @param shapes Either a single {sf} polygon object, or a list thereof, to map
#'
#' @return A dataframe describing the max boundaries of the submitted shapes
#'
#' @examples t = data.frame(l = 50, ln = -120) |> sf::st_as_sf(coords = c('ln','l'), crs = 4326)
#' b = data.frame(l = 49, ln = -120) |> sf::st_as_sf(coords = c('ln','l'), crs = 4326)
#' li = list(t, b)
#' find_shapes_max_ext(li)
#'
find_shapes_max_ext = function(shapes){
  # # Test that shapes are in the same CRS.
  # shapes_crs = lapply(shapes, \(x) data.frame(crs = stringr::str_extract(as.character(sf::st_crs(x))[1],"EPSG\\:[0-9]+"))) |>
  #   dplyr::bind_rows()
  #
  # if(nrow(dplyr::distinct(shapes_crs))>1) stop("Shapes are not in the same Coordinate Reference System (i.e. projection); please ensure all shapes are in one CRS.")

  lapply(shapes, \(x){
    bounds = sf::st_bbox(x)
    out = data.frame(xmin = bounds[1],
                     xmax = bounds[3],
                     ymin = bounds[2],
                     ymax = bounds[4])

    row.names(out) <- NULL
    out
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(xmin = min(xmin,na.rm=T),
                  xmax = max(xmax,na.rm=T),
                  ymin = min(ymin,na.rm=T),
                  ymax = max(ymax,na.rm=T)) |>
    dplyr::distinct()
}
