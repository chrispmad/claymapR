#' Make a 3D "Clay" Map
#'
#' @param shape_colour_var Variable name on which to base colour fill or border of shapes
#' @param colour_type Should the shapes' border or fill be coloured? One of 'fill','border'
#' @param colour_alpha Opacity of shape fill or border colour; defaults to 0.3 (30%)
#' @param material The material to mold the 3d map from; one of 'clay', 'satellite'
#' @param map_detail The level of detail for the map; min is 1 and max is 14, defaults to 6
#' @param elevation_map A {terra} raster of elevation for the area of interest; default is NULL and will calculate
#' @param distance_map A {terra} raster of distance from extended map base cells to the shapes to map; default is NULL and will calculate
#' @param take_snapshot Shall a 2D snapshot be taken of the 3d RGL widget? Defaults to TRUE
#' @param snapshot_filename What filename shall be used for the snap? Default is system date.
#'
#' @return A 3D map RGL widget or snapshot thereof
#' @export
#'
#' @examples \dontrun
claymap3d = function(prepped_clay,
                     shape_colour_var = NULL,
                     colour_type = c("fill"),
                     colour_alpha = 0.3,
                     material = 'clay',
                     take_snapshot = TRUE,
                     snapshot_filename = NULL
){

  rgl::close3d()

  shapes = prepped_clay$shapes
  elevation_map = prepped_clay$elev_map
  dist_to_border = prepped_clay$dist_to_border
  map_detail = prepped_clay$map_detail
  mbase = prepped_clay$mbase
  elev = prepped_clay$elev
  elev_mb = prepped_clay$elev_mb

  shape_sum = dplyr::summarise(shapes)

  # Grab elevation map, if supplied.
  # The user has opted for a "clay" map - i.e. using elevation!
  if(!is.null(elevation_map)){
    elev = elevation_map
  } else {
    # Grab elevation data.
    elev = terra::rast(
      suppressMessages(
        elevatr::get_elev_raster(
        locations = shape_sum,
        z = map_detail)
      )
    )
  }

  # Calculate distance to border, or read in raster :)
  # Find distance for framing map base portion to elevation values within shape.

  # Do elevation stuff
  elev_c = terra::crop(terra::mask(elev, shape_sum), shape_sum)

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

  # Combine the satellite imagery with the shapes; save to disk?
  # image_filepath = 'texture_image.png'

  image_filepath = tempfile()

  grDevices::png(filename = image_filepath, width = 10, height = 10, units = 'in', res = 300)

  if(colour_type == 'fill'){
    overlay = ggplot2::ggplot() +
      ggplot2::geom_sf(data = sf::st_transform(shapes,terra::crs(elev_mb)),
                       ggplot2::aes(fill = !!rlang::sym(shape_colour_var)),
                       alpha = colour_alpha) +
      ggthemes::theme_map() +
      ggplot2::theme(legend.position = 'none',
                     plot.background = ggplot2::element_rect(fill = 'transparent',
                                                             colour = 'transparent'))
  }
  if(colour_type == 'border'){
    overlay = ggplot2::ggplot() +
      ggplot2::geom_sf(data = sf::st_transform(shapes,terra::crs(elev_mb)),
                       ggplot2::aes(colour = !!rlang::sym(shape_colour_var),
                                    fill = !!rlang::sym(shape_colour_var)),
                       alpha = colour_alpha,
                       linewidth = 1.5
      ) +
      ggthemes::theme_map() +
      ggplot2::theme(legend.position = 'none',
        plot.background = ggplot2::element_rect(fill = 'transparent',
                                                colour = 'transparent'))
  }
  full_plot = gg_baselayer +
    constrain_ggplot +
    patchwork::inset_element(overlay + constrain_ggplot,
                             left = 0, bottom = 0,
                             right = 1, top = 1, align_to = 'full')

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

  # Open a big RGL window
  rgl::open3d(windowRect = c(100,100,1200,800))
  # Plot the clay map in the RGL window
  rgl::clear3d(); rgl::rglwidget(reuse = TRUE); rgl::shade3d(qmesh, specular = "transparent"); rgl::aspect3d(1, 1, 0.09);
  # Adjust RGL window's focus
  rgl::view3d(theta = 0, phi = -50, fov = 10, zoom = 0.55)

  if(take_snapshot){
    if(is.null(snapshot_filename)){
      snapshot_filename = paste0("claymap_",Sys.Date(),"_",colour_type,"_colour.png")
    }
    rgl::rgl.snapshot(filename = snapshot_filename)
    cat(paste0("\nSnapshot file saved to ",snapshot_filename,"\n"))
  }

  if(take_snapshot){
    return(magick::image_read(snapshot_filename))
  }
}
