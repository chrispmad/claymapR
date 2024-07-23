#' Mold a Clay Map with Shapes on top
#'
#' @param prepped_clay This claymapR list object includes prepared data
#' @param base_map_alpha How transparent should the basemap be? Defaults to 0.5; higher values are more opaque.
#' @param shape_border_colour Which colour should delineate polygon borders?
#' @param shape_fill_vars Variable names on which to base colour/fill of points/shapes respectively
#' @param palettes Which palettes should be used for shapes' colour or fill?
#' @param shape_fill_alphas How transparent should the shapes' colour(s)/fill(s) be?
#' @param background Should an 'ocean' background be added?
#'
#' @return A ggplot2 plot of your shape(s) with a basemap below.
#' @export
#'
#' @examples \dontrun
mold_claymap = function(prepped_clay,
                        base_map_alpha = 0.5,
                        shape_border_colour = 'grey',
                        shape_fill_vars = NULL,
                        palettes = c('Spectral'),
                        shape_fill_alphas = 0.1,
                        background = TRUE){

  shapes = prepped_clay$shapes
  elevation_map = prepped_clay$elev_map
  map_detail = prepped_clay$map_detail
  elev = prepped_clay$elev
  elev_mb = prepped_clay$elev_mb
  mbase = prepped_clay$mbase

  requireNamespace("patchwork")

  if(is.null(shape_fill_vars)) stop("Please specify the column name for shape fill colour")

  if(is.null(map_detail)){
    map_detail = 6
  }

  # Ensure we have enough values for border colours, alpha, and palettes

  if(length(shape_border_colour) < length(shapes)){
    shape_border_colour = rep(shape_border_colour,length(shapes))
  }
  if(length(shape_fill_alphas) < length(shapes)){
    shape_fill_alphas = rep(shape_fill_alphas,length(shapes))
  }
  if(length(palettes) < length(shapes)){
    palettes = rep(palettes, length(shapes))
  }
  # shape_sum = dplyr::summarise(shapes)

  # Make map base (extends polygons to be mapped by proportion of polygons' longest dimension)
  # mbase = make_map_base(shape_sum, buffer = 0.05)

  # Make 'map table' - a space for hard shadows to be mapped.
  # mtable = make_map_base(shape_sum, buffer = 0.5)

  # Map bbox based on watershed shapes.
  # map_bb = sf::st_as_sfc(sf::st_bbox(shape_sum))
  # map_bb = mbase

  # Get basemaps
  ext <- sf::st_transform(mbase, 3857)

  # # The user has opted for a "clay" map - i.e. using elevation!
  # if(!is.null(elevation_map)){
  #   elev = elevation_map
  # } else {
  #   # Grab elevation data.
  #   elev = terra::rast(
  #     suppressMessages(
  #       elevatr::get_elev_raster(
  #         locations = shape_sum,
  #         z = map_detail)
  #     )
  #   )
  # }

  names(elev) <- 'elev'

  # Crop elevation for just our area of interest.
  # elev_c = terra::crop(terra::mask(elev, shape_sum), shape_sum)
  # elev_c = elev_mb

  names(elev_mb) <- 'elev'

  map_w = sf::st_bbox(mbase)[3] - sf::st_bbox(mbase)[1]
  map_h = sf::st_bbox(mbase)[4] - sf::st_bbox(mbase)[2]

  max_d = ifelse(map_w > map_h, map_w, map_h)

  max_shadow_length = max_d * 0.02

  # Initialize a hard shadow layer
  shadow_layer = elev_mb
  shadow_layer[!is.na(shadow_layer)] <- 1

  # Shift the shadow layer by max_shadow_length
  shadow_layer_shift = terra::shift(shadow_layer, dx = -max_shadow_length, dy = -max_shadow_length)

  shadow_poly_shift = terra::as.polygons(shadow_layer_shift)
  shadow_poly = terra::as.polygons(shadow_layer)

  shadow_poly = sf::st_as_sf(shadow_poly)
  shadow_poly_shift = sf::st_as_sf(shadow_poly_shift)

  # Drop portions of the hard shadow polygon that intersect with the shapes to be plotted.
  shadow_polys = dplyr::bind_rows(shadow_poly_shift,shadow_poly)

  overlaps <- sf::st_intersection(shadow_polys)

  non_overlapping_shadow = overlaps |>
    dplyr::filter(n.overlaps == 1) |>
    dplyr::mutate(origins = unlist(origins)) |>
    dplyr::filter(origins == 1)

  # Convert to points, and skew slightly left and down?

  # Make hillshade
  slope <- terra::terrain(elev_mb, "slope", unit="radians")
  aspect <- terra::terrain(elev_mb, "aspect", unit="radians")
  hill <- terra::shade(slope, aspect, 45, 45)
  names(hill) <- 'hillshade'

  # Accentuate hillshade
  hill = hill ^ 2

  # Make hillshade layer
  hill_gg = ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = as.data.frame(hill, xy = TRUE),
      ggplot2::aes(
        x=x,y=y,
        fill = hillshade,
        alpha = hillshade
      )
    ) +
    ggplot2::scale_fill_gradient(
      low = 'black',
      high = 'white',
      na.value = 'transparent') +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position = 'none')  +
    ggplot2::coord_sf(xlim = sf::st_bbox(mbase)[c(1,3)],
                      ylim = sf::st_bbox(mbase)[c(2,4)])

  # Make shadow layer.
  shadow_gg = ggplot2::ggplot() +
    ggplot2::geom_sf(data = shadow_poly_shift,
                     fill = '#1c1b1a',
                     alpha = 0.75) +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position = 'none')  +
    ggplot2::coord_sf(xlim = sf::st_bbox(mbase)[c(1,3)],
                      ylim = sf::st_bbox(mbase)[c(2,4)])

  # Make elevation-derived clay layer. We'll add chunky shadows (if map shape is chunky)
  # and hillshade over top this layer.
  clay_layer = ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = elev_mb, ggplot2::aes(fill = elev)) +
    ggplot2::scale_fill_gradient2(low = "#3b2f1f", mid = "#b8a997",
                                  high = "#e3d0b8",
                                  midpoint = max(terra::values(elev_mb),na.rm=T)/2,
                                  na.value = 'transparent')  +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::coord_sf(xlim = sf::st_bbox(mbase)[c(1,3)],
                      ylim = sf::st_bbox(mbase)[c(2,4)])

  # Add shapes over top. One layer per shape?
  shape_layers = list()

  for(i in 1:length(shapes)){

    # Depending on geometry of each shape, use colour input
    # to set either the colour or fill of the shape.
    p <- ggplot2::ggplot()

    p <- p
      # ggplot2::geom_sf(data = shapes[[i]], ggplot2::aes(fill = !!rlang::sym(shape_fill_vars[[i]])),
      #                  col = shape_border_colour[[i]], alpha = shape_fill_alphas[[i]]) +

    # Is the colour palette continuous or discrete?

    if(unique(sf::st_geometry_type(shapes[[i]]))[1] %in% c("POINT","LINESTRING")){
      # Points / linestrings detected; apply colour var and palette to 'color' aesthetic.
      p <- p +
        ggplot2::geom_sf(data = shapes[[i]], ggplot2::aes(color = !!rlang::sym(shape_fill_vars[[i]])),
                         alpha = shape_fill_alphas[[i]])
    }
    if(unique(sf::st_geometry_type(shapes[[i]]))[1] %in% c("POLYGON","MULTIPOLYGON")){
      # polygons detected; apply colour var and palette to 'color' aesthetic.
      p <- p +
        ggplot2::geom_sf(data = shapes[[i]], ggplot2::aes(fill = !!rlang::sym(shape_fill_vars[[i]])),
                         col = shape_border_colour[[i]], alpha = shape_fill_alphas[[i]])
    }

    p = p +
      ggplot2::scale_colour_brewer(palette = palettes[[i]]) +
      ggthemes::theme_map() +
      ggplot2::theme(legend.position = 'right') +
      ggplot2::labs(fill = 'Identity') +
      ggplot2::coord_sf(xlim = sf::st_bbox(mbase)[c(1,3)],
                        ylim = sf::st_bbox(mbase)[c(2,4)]) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    # I haven't figured out how to add legends successfully; for now,
    # don't include!
    # if(!include_legends[[i]]) {
      p = p +
        ggplot2::theme(legend.position = 'none')
    # }

    shape_layers[[i]] <- p
  }
  # ws_overlay = ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = shapes[[i]], ggplot2::aes(fill = !!rlang::sym(shape_fill_vars[[1]])),
  #                    col = shape_border_colour, alpha = shape_fill_alphas[[1]]) +
  #   ggthemes::theme_map() +
  #   ggplot2::labs(fill = 'Identity') +
  #   ggplot2::coord_sf(xlim = sf::st_bbox(mbase)[c(1,3)],
  #                     ylim = sf::st_bbox(mbase)[c(2,4)]) +
  #   ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))

  if(background){
    background = ggplot2::ggplot() +
      ggplot2::geom_sf(data = sf::st_as_sfc(sf::st_bbox(mbase)),
                       fill = 'lightblue', col = 'lightblue') +
      ggthemes::theme_map() +
      ggplot2::coord_sf(xlim = sf::st_bbox(mbase)[c(1,3)],
                        ylim = sf::st_bbox(mbase)[c(2,4)],
                        expand = F)
  } else {
    background = ggplot2::ggplot()
  }

  # browser()

  full_plot = background +
    patchwork::inset_element(shadow_gg, left = 0, bottom = 0,
                             right = 1, top = 1, align_to = 'full') +
    patchwork::inset_element(clay_layer, left = 0, bottom = 0,
                             right = 1, top = 1, align_to = 'full') +
    patchwork::inset_element(hill_gg, left = 0, bottom = 0,
                             right = 1, top = 1, align_to = 'full') #+
    # patchwork::inset_element(ws_overlay, left = 0, bottom = 0,
    #                          right = 1, top = 1, align_to = 'full')
  for(i in 1:length(shapes)){
    full_plot = full_plot +
      patchwork::inset_element(shape_layers[[i]], left = 0, bottom = 0,
                               right = 1, top = 1, align_to = 'full')
  }
  return(full_plot)
}
