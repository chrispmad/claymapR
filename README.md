
<!-- README.md is generated from README.Rmd. Please edit that file -->

# claymapR

<!-- badges: start -->
<!-- badges: end -->

The goal of {claymapR} is to help you make aesthetically pleasing maps
with a couple lines of code!

## Installation

You can install the development version of {claymapR} like so:

``` r
devtools::install_github('chrispmad/claymapR')
```

## Example Usage

Let’s say you have the following data.

``` r
library(claymapR)
library(bcdata)
library(patchwork)

# Make bounding box.
sample_bbox = data.frame(x = c(1582344.6,1820447.3),
           y = c(521730.1, 771002.9)) |> 
  sf::st_as_sf(coords = c('x','y'), crs = 3005) |> 
  sf::st_bbox()

# A handful of choice watersheds in British Columbia.
ws = bcdata::bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  bcdata::filter(bcdata::INTERSECTS(sample_bbox)) |>
  bcdata::filter(!WATERSHED_GROUP_NAME %in% c("Upper Arrow Lake","Lower Arrow Lake",
                                      "Slocan River","Kootenay Lake",
                                      "Duncan Lake")) |>
  bcdata::collect() |> 
  dplyr::rename(ws_name = WATERSHED_GROUP_NAME)

# Join together those watersheds into a bounding polygon.
ws_borders = ws |>
  dplyr::summarise()

# Take a glance at the watersheds.
ggplot2::ggplot() + 
  ggplot2::geom_sf(data = ws)
```

<img src="man/figures/README-prep_data-1.png" width="100%" />

Step one is to “prep the clay”. This step downloads elevation data,
calculates a distance matrix, and some other small steps.

``` r
my_clay = prep_clay(ws)
#> 
#> Digging up clay...
#> 
#> Kneading clay...
#> |---------|---------|---------|---------|=========================================                                          
#> Clay prepped!
```

Using the prepped ‘clay’, you can make a two-dimensional ‘clay’ map like
this:

``` r
claymapR::mold_claymap(prepped_clay = my_clay, 
                       shape_fill_var = 'ws_name')
```

<img src="man/figures/README-2d_claymap-1.png" width="100%" />

Or you can make a three-dimensional ‘clay’ map like this. Note that when
you run this function in an interactive session, the 3-D visualizer RGL
window remains open, allowing you to rotate the map and take subsequent
snapshots with `rgl::rgl.snapshot(filename = "some_name.png")`.

``` r
claymap3d(my_clay, shape_colour_var = 'ws_name')
#> 
#> Snapshot file saved to claymap_2024-06-11_fill_colour.png
```

<img src="man/figures/README-3d_claymap-1.png" width="100%" />

You can set the colour to the polygon border rather than the fill.

``` r
claymap3d(my_clay,
          shape_colour_var = 'ws_name',
          colour_type = 'border')
#> 
#> Snapshot file saved to claymap_2024-06-11_border_colour.png
```

<img src="man/figures/README-3d_claymap_border_colour-1.png" width="100%" />

``` r
claymap3d(my_clay, 
          shape_colour_var = 'ws_name',
          colour_type = 'border',
          material = 'satellite'
)
#> Loading basemap 'world_imagery' from map service 'esri'...
#> 
#> Snapshot file saved to claymap_2024-06-11_border_colour.png
```

<img src="man/figures/README-3d_claymap_satellite-1.png" width="100%" />
