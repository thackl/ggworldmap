#' Create a ggworldmap with graticules and degrees
#'
#' `ggworldmap()` is a short cut to create a [ggplot2::ggplot] with polygons of
#' a world map polygons, graticules and degree labels. It also adds a customized
#' theme and a fixed aspect ratio coordinate system ([ggplot2::coord_equal]).
#'
#' `lat_min/lat_max` default to 84°S/W, respectively. This is mainly because the
#' polygon map that comes with ggplot2 only covers that range. Also, in most
#' projections, those extreme latitudes are heavily distorted and plotting them
#' doesn't make sense anyway.
#' 
#' @inheritParams ggplot2::ggplot
#' @inheritParams ggworldmap::geom_worldmap
#' @param style Default style for the map, graticules, degrees and the default
#' theme. Choices: "light".
#' @param graticule Style and arguments passed on to [geom_graticule]. Either a
#' single style string, a list of args with the style as first argument, or NULL
#' to disable graticules.
#' @param gratframe Style and arguments passed on to [geom_gratframe]. Either a
#' single style string, a list of args with the style as first argument, or NULL
#' to disable a gratiframe.
#' @param degree Style and arguments passed on to [geom_degree]. Either a single
#' style string, a list of args with the style as first argument, or NULL to
#' disable degrees.
#' @param theme Style and arguments passed on to `theme_worldmap_<style>`
#' themes. Either a single style string, a list of args with the style as first
#' argument, or NULL to disable a default theme.
#' @param ... Other arguments passed on to [geom_worldmap]
#' @export
#' @return ggplot object
#' @examples
#' ggworldmap()
#'
#' ggworldmap(proj = "robin", degree = NULL)
#'
#' ggworldmap(proj = "merc", long_0 = -90, lat_min = -75, lat_max = 75,
#'   mapping = aes(fill=region), color = "black",
#'   graticule = list("light", color = "grey50"))
ggworldmap <- function(data = NULL, mapping = aes(x=long, y=lat), map = "world",
    proj = NULL, long_0 = 0, long_min = -180 + long_0, long_max = 180 + long_0, 
    lat_min = -84, lat_max = 84, proj_extra = NULL, style = c("light"), ...,
    graticule = style, gratframe = style, degree = style, theme = style,
    environment = parent.frame()){

  # from match.arg
  styles <- eval(formals()$style)
  match.arg(style, styles)
  
  p <- ggplot2::ggplot(data = data, mapping = mapping, environment = environment)

  # map (store args shared with other geoms)
  worldmap_shared_args <- list(proj = proj, long_0 = long_0, long_min = long_min,
      long_max = long_max, lat_min = lat_min, lat_max = lat_max,
      proj_extra = proj_extra)

  worldmap_defs <- list(
    light = list(color = "gray90", fill = "gray90")
  )[[style]]

  worldmap_args <- modifyList(worldmap_defs, c(list(..., mapping = mapping, map = map), worldmap_shared_args))
  
  p <- p + do.call(geom_worldmap, worldmap_args)
  
  # graticules
  graticule_style <- graticule[[1]] %&&% match.arg(graticule[[1]], styles)
  if(!is.null(graticule_style)){ # add gggenomes graticule
    graticule_args <- if(is.list(graticule) && length(graticule) >1) graticule[-1] else list()
    graticule_defs <- list(light = list(color = "gray80"))[[graticule_style]]
    graticule_defs <- modifyList(graticule_defs, worldmap_shared_args)
    graticule_args <- modifyList(graticule_defs, graticule_args)
      p <- p + do.call(geom_graticule, graticule_args)
    # p <- p + do.call(geom_gratframe, graticule_args)
  }

  # gratframe
  gratframe_style <- gratframe[[1]] %&&% match.arg(gratframe[[1]], styles)
  if(!is.null(gratframe_style)){ # add gggenomes gratframe
    gratframe_args <- if(is.list(gratframe) && length(gratframe) >1) gratframe[-1] else list()
    gratframe_defs <- list(light = list(color = "gray80"))[[gratframe_style]]
    gratframe_defs <- modifyList(gratframe_defs, worldmap_shared_args)
    gratframe_args <- modifyList(gratframe_defs, gratframe_args)
      p <- p + do.call(geom_gratframe, gratframe_args)
    # p <- p + do.call(geom_gratframe, graticule_args)
  }

  # degrees
  degree_style <- degree[[1]] %&&% match.arg(degree[[1]], styles)
  if(!is.null(degree_style)){ # add gggenomes degree
    degree_args <- if(is.list(degree) && length(degree) >1) degree[-1] else list()
    degree_args <- if(is.list(degree) && length(degree) >1) degree[-1] else list()
    degree_defs <- list(light = list(color = "gray80"))[[degree_style]]
    degree_defs <- modifyList(degree_defs, worldmap_shared_args)
    degree_args <- modifyList(degree_defs, degree_args)
    p <- p + do.call(geom_degree, degree_args)
  }

  # theme
  theme_style <- theme[[1]] %&&% match.arg(theme[[1]], styles)
  if(!is.null(theme_style)){ # add theme
    theme_args <- if(is.list(theme) && length(theme) >1) theme[-1] else list()
    p <- p + do.call(paste0("theme_worldmap_", theme), theme_args)
  }
  
  p + coord_equal()
}
