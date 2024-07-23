#' Make a 3D "Clay" Map
#'
#' @param prepped_clay The output of a "prep_clay()" function call.
#' @param shape_fill_vars Variable name on which to base colour fill or border of shapes
#' @param colour_type Should the shapes' border or fill be coloured? One of 'fill','border'
#' @param colour_alpha Opacity of shape fill or border colour; defaults to 0.3 (30%)
#' @param material The material to mold the 3d map from; one of 'clay', 'satellite'
#' @param return_qmesh Should this function return a 'qmesh' object?
#' @param take_snapshot Shall a 2D snapshot be taken of the 3d RGL widget? Defaults to TRUE
#' @param snapshot_filename What filename shall be used for the snap? Default is system date.
#'
#' @return A 3D map RGL widget to interact with; optionally, also a snapshot and a 'qmesh' object
#' @export
#'
#' @examples \dontrun
claymap3d = function(prepped_clay,
                     shape_fill_vars = NULL,
                     palettes = c('Spectral'),
                     shape_fill_alphas = 0.1,
                     shape_border_colour = 'grey',
                     # colour_type = c("fill"),
                     # colour_alpha = 0.3,
                     material = 'clay',
                     return_qmesh = TRUE,
                     take_snapshot = FALSE,
                     snapshot_filename = NULL
){

  if(is.null(shape_fill_vars)) stop("Sorry - please give the column name to colour the shape by as 'shape_fill_vars'!")

  rgl::close3d()

  shapes = prepped_clay$shapes
  elev = prepped_clay$elev
  dist_to_border = prepped_clay$dist_to_border
  map_detail = prepped_clay$map_detail
  mbase = prepped_clay$mbase
  elev = prepped_clay$elev
  elev_mb = prepped_clay$elev_mb

  # shape_sum = dplyr::summarise(shapes)

  # Grab elevation map, if supplied.
  # The user has opted for a "clay" map - i.e. using elevation!
  # if(is.null(elevation_map)){
  #   # Grab elevation data.
  #   elev = terra::rast(
  #     suppressMessages(
  #       elevatr::get_elev_raster(
  #       locations = shape_sum,
  #       z = map_detail)
  #     )
  #   )
  # }

  # Calculate distance to border, or read in raster :)
  # Find distance for framing map base portion to elevation values within shape.

  # Do elevation stuff
  # elev_c = terra::crop(terra::mask(elev, shape_sum), shape_sum)

  # mbase = make_map_base(shape_sum, buffer = 0.1)
  #
  # elev_mb = terra::crop(terra::mask(elev, mbase), mbase)

  # Expand our elevation raster to the elevation model base extent, use distance function to
  # infer dropping elevation values.
  dist_mb = terra::mask(dist_to_border, elev_mb)

  # Standardize distances to scale from 0 to 1; take inverse of values.
  largest_dist = max(terra::values(dist_mb),na.rm=T)

  # Convert to 'decay' values
  decay_values <- 1 - (dist_mb / largest_dist)
  decay_values[decay_values < 0] <- 0
  names(decay_values) <- 'decay'

  # Accentuate decay
  decay_values = decay_values^2

  # Apply decay to elevation values
  elev_mb <- elev_mb * decay_values

  # If we want a way to constrain ggplots, we can use this
  constrain_ggplot = ggplot2::coord_sf(
    xlim = terra::ext(elev_mb)[c(1,2)],
    ylim = terra::ext(elev_mb)[c(3,4)]
  )

  mbase_width = sf::st_bbox(mbase)[3] - sf::st_bbox(mbase)[1]
  mbase_height = sf::st_bbox(mbase)[4] - sf::st_bbox(mbase)[2]

  big_dimension = ifelse(mbase_width >= mbase_height, mbase_width, mbase_height)

  # Grab satellite imagery
  ext_bbox = mbase |>
    # Buffer this base enough to have enough satellite imagery after reprojecting.
    sf::st_buffer(dist = big_dimension*0.2) |>
    sf::st_transform(3857) |>
    sf::st_bbox()

  if(material == 'satellite'){

    sat_r = basemaps::basemap_terra(ext_bbox, map_service = 'esri', map_type = 'world_imagery') |>
      methods::as("SpatRaster")

    # Reproject satellite imagery into the CRS of the elev_mb object.
    sat_r = terra::project(sat_r, terra::crs(elev_mb))

    # Crop, resample and mask satellite imagery by elev_mb.
    sat_r = terra::crop(sat_r, elev_mb)

    sat_r = terra::resample(sat_r, elev_mb)

    sat_r = terra::mask(sat_r, elev_mb)

    sat_R = raster::raster(sat_r$red)
    sat_G = raster::raster(sat_r$green)
    sat_B = raster::raster(sat_r$blue)

    sat_raster = raster::stack(sat_R,sat_G,sat_B)

    gg_baselayer = ggplot2::ggplot() +
      tidyterra::geom_spatraster_rgb(data = sat_r, alpha = 1) +
      ggplot2::coord_sf(expand = FALSE) +
      ggthemes::theme_map() +
      ggplot2::theme(legend.position = 'none')
  }
  if(material == 'clay'){
    baselayer = terra::shade(
      terra::terrain(elev_mb, "slope", unit="radians"),
      terra::terrain(elev_mb, "aspect", unit="radians"),
      45, 45
    )
    names(baselayer) <- 'hillshade'

    gg_baselayer = ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = baselayer) +
      ggplot2::scale_fill_gradient2(low = '#9a3517', mid = '#fa883f', high = '#f7e0aa',
                                    midpoint = 0.5, na.value = 'transparent') +
      ggplot2::coord_sf(expand = FALSE) +
      ggthemes::theme_map() +
      ggplot2::theme(legend.position = 'none')
  }

  # Combine the satellite/clay imagery with the shapes; save to disk?
  # image_filepath = 'texture_image.png'

  image_filepath = tempfile()

  grDevices::png(filename = image_filepath, width = 10, height = 10, units = 'in', res = 300)

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
  # if(colour_type == 'fill'){
    # overlay = ggplot2::ggplot() +
    #   ggplot2::geom_sf(data = sf::st_transform(shapes,terra::crs(elev_mb)),
    #                    ggplot2::aes(fill = !!rlang::sym(shape_fill_vars)),
    #                    alpha = shape_fill_alphas) +
    #   ggthemes::theme_map() +
    #   ggplot2::theme(legend.position = 'none',
    #                  plot.background = ggplot2::element_rect(fill = 'transparent',
    #                                                          colour = 'transparent'))
  # }
  # if(colour_type == 'border'){
  #   overlay = ggplot2::ggplot() +
  #     ggplot2::geom_sf(data = sf::st_transform(shapes,terra::crs(elev_mb)),
  #                      ggplot2::aes(colour = !!rlang::sym(shape_fill_vars),
  #                                   fill = !!rlang::sym(shape_fill_vars)),
  #                      alpha = shape_fill_alphas,
  #                      linewidth = 1.5
  #     ) +
  #     ggthemes::theme_map() +
  #     ggplot2::theme(legend.position = 'none',
  #       plot.background = ggplot2::element_rect(fill = 'transparent',
  #                                               colour = 'transparent'))
  # }
  full_plot = gg_baselayer +
    constrain_ggplot

  for(i in 1:length(shapes)){
    full_plot = full_plot +
      patchwork::inset_element(shape_layers[[i]], left = 0, bottom = 0,
                               right = 1, top = 1, align_to = 'full')
  }

  print(full_plot)

  grDevices::dev.off()

  # Remove whitespace margins from raster
  magick::image_read(image_filepath) |>
    magick::image_trim() |>
    magick::image_write(image_filepath)

  # Read the combined RGB raster image back in as a rast
  surface_img_rast = suppressWarnings(terra::rast(image_filepath))

  # Set layer names
  names(surface_img_rast) <- c("R", "G", "B")

  # Update the extent and CRS
  terra::ext(surface_img_rast) = terra::ext(elev_mb)
  terra::crs(surface_img_rast) = terra::crs(elev_mb)

  si_r = suppressWarnings(raster::raster(surface_img_rast$R))
  si_g = suppressWarnings(raster::raster(surface_img_rast$G))
  si_b = suppressWarnings(raster::raster(surface_img_rast$B))

  surface_texture = suppressWarnings(raster::stack(si_r,si_g,si_b))

  qmesh <- suppressWarnings(
    suppressMessages(
      quadmesh::quadmesh(
        suppressWarnings(raster::raster(elev_mb)),
        texture = suppressWarnings(surface_texture))
    )
  )

  # Calculate the map width, and from that, the size of markers.
  elev_ext = as.data.frame(as.matrix(terra::ext(elev_mb)))
  width_of_map = elev_ext$max[1] - elev_ext$min[1]
  marker_size = width_of_map / 100

  # Function to add a balloon to each marker location.
  add_balloon_marker <- function(x, y, z, size = 10, col_var) {
    x = as.numeric(x); y = as.numeric(y); z = as.numeric(z)
    spheres3d(x, y, z, radius = size, color = col_var)
    # Add the string of the balloon as a line
    lines3d(c(x, x), c(y, y), c(z + z*0.2, z), color = "black")
  }

  # Open a big RGL window
  rgl::open3d(windowRect = c(100,100,1200,800))
  # Plot the clay map in the RGL window
  rgl::clear3d(); rgl::rglwidget(reuse = TRUE); rgl::shade3d(qmesh, specular = "transparent"); rgl::aspect3d(1, 1, 0.09);
  # Adjust RGL window's focus
  rgl::view3d(theta = 0, phi = -50, fov = 10, zoom = 0.55)

  # Take marker layers and convert them to matrices to be added
  # to RGL window manually.
  for(i in 1:length(shapes)){
    # Test if this layer could be a marker; use geometry type.
    if(unique(sf::st_geometry_type(shapes[[i]]))[1] == 'POINT'){
      marks = shapes[[i]]
      # Extract elevation from elev_mb raster.
      marks$elev = terra::extract(elev_mb, terra::vect(marks))[,2]

      # Make into dataframe with a colour variable column.
      marks = marks |>
        dplyr::mutate(lat = sf::st_coordinates(geometry)[,2],
                      lng = sf::st_coordinates(geometry)[,1]) |>
        sf::st_drop_geometry() |>
        dplyr::select(lat,lng,elev,!!rlang::sym(shape_fill_vars[i])) |>
        dplyr::mutate(col_var = leaflet::colorFactor(palette = palettes[i], domain = unique(marks$LONG_TYPE))(!!rlang::sym(shape_fill_vars[i])))

      # Once the RGL window is open, cycle through all markers, adding them
      # to the map one-by-one.
      apply(marks, 1, function(coord) {
        add_balloon_marker(coord[2], coord[1], coord[3],
                           size = marker_size, coord[5])
      })
    }
  }

  if(take_snapshot){
    if(is.null(snapshot_filename)){
      snapshot_filename = paste0("claymap_",Sys.Date(),".png")
    }
    rgl::rgl.snapshot(filename = snapshot_filename)
    cat(paste0("\nSnapshot file saved to ",snapshot_filename,"\n"))
  }

  # Convert our RGL window to a widget; we can export this from the function.
  widget <- rgl::rglwidget()

  return(widget)
  # if(return_qmesh){
  #   return(qmesh)
  # }
}
