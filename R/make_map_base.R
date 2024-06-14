#' Make map 'base'
#'
#' @param shapes polygons to be mapped
#' @param buffer How far should the 'base' extend from the polygons to be mapped, as percentage of largest X or Y dimension?
#'
#' @return An sf polygon the represents the 'base' shape
#' @export
#'
#' @examples \dontrun
make_map_base = function(shapes, buffer = 5){
  # Check that the shape(s) are already summarised; if not, do that here.
  if(nrow(shapes) > 1){
    shapes = dplyr::summarise(shapes)
  }
  # Calculate width and height of shape.
  shape_w = sf::st_bbox(shapes)[3] - sf::st_bbox(shapes)[1]
  shape_h = sf::st_bbox(shapes)[4] - sf::st_bbox(shapes)[2]

  if(buffer >= 1){
    buffer = buffer / 100
  }

  # Find which of the two dimensions is larger
  if(shape_w > shape_h){
    big_dimension = shape_w
  } else {
    big_dimension = shape_h
  }
  mbase = sf::st_buffer(shapes, dist = big_dimension * buffer)

  return(mbase)
}
