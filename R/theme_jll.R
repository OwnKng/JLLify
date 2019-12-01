jll_colors <- c(
  'jll_red' = "#E30613",
  'black' = "#000000",
  'steel' = "#626468",
  'concrete' = "#B1B2B4",
  'stone' = "#DBD6C7",
  'white' = "#FFFFFF",
  'violet' = "#7874B5",
  'purple' = "#602375",
  'raspberry' = "#9A054A",
  'orange' = "#ED700A",
  'water' = "#D3E3EA",
  'olive' = "#BBCEA4"
)

#' @title Apply the JLL palette to a plot
#'
#' @description This function generates the JLL color palettes. It is used by the scale_color_jll() functions, and rarely needs to be called directly.
#' @export
#' @examples
#' jll_pal()

jll_pal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- jll_pallete[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' @title Apply the JLL palette to a plot
#'
#' @description This function returns the colors in the JLL palettes. It is used by the scale_color_jll() functions, and rarely needs to be called directly.
#' @export
#' @examples
#' jll_cols()

jll_cols <- function(...) {
  cols <- c(...)

  if(is.null(cols))
    return(jll_colors)

  jll_colors[cols]
}

jll_pallete <- list(
  'primary'  = jll_cols("jll_red", "black", "concrete","steel", "stone"),
  'primary_3' = jll_cols("jll_red", "black", "stone"),
  'primary_3_dark' = jll_cols("jll_red", "stone", "violet"),
  'secondary' = jll_cols("violet","purple","raspberry","orange"),
  'colorfull' = jll_cols("steel", "stone","jll_red", "violet","purple","raspberry","orange"),
  'primary_secondary' = jll_cols("jll_red", "black", "concrete","steel", "stone","violet","purple","raspberry","orange"),
  'ten_cols' = jll_cols("jll_red", "steel", "stone", "concrete", "violet", "purple",
                        "raspberry", "orange", "water", "olive")
)

#' @title Apply JLL colors to a plot
#'
#' @description This function applies one of JLL's corporate color palletes to a ggplot object.
#' @param palette the name of the color palette you wish to apply, defaults to "primary". Other supported palettes are "primary_3", "primary_3_dark", "secondary", "colorfull", "primary_secondary" and "ten_cols"
#' @param discrete whether the variable the palette being applied to is discrete. Defaults to TRUE.
#' @param reverse whether to reverse the color palette. Defaults to FALSE.
#' @export
#' @examples
#' scale_color_jll()

scale_color_jll <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jll_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @title Apply JLL colors to a plot
#'
#' @description This function applies one of JLL's corporate color palletes to a ggplot object. It is the same as scale_color_jll(), but with the English spelling of "colour".
#' @param palette the name of the color palette you wish to apply, defaults to "primary". Other supported palettes are "primary_3", "primary_3_dark", "secondary", "colorfull", "primary_secondary" and "ten_cols"
#' @param discrete whether the variable the palette being applied to is discrete. Defaults to TRUE.
#' @param reverse whether to reverse the color palette. Defaults to FALSE.
#' @export
#' @examples
#' scale_colour_jll()

scale_colour_jll <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jll_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @title Apply JLL fill to a plot
#'
#' @description This function applies one of JLL's corporate color palletes to a ggplot object.
#' @param palette the name of the color palette you wish to apply, defaults to "primary". Other supported palettes are "primary_3", "primary_3_dark", "secondary", "colorfull", "primary_secondary" and "ten_cols"
#' @param discrete whether the variable the palette being applied to is discrete. Defaults to TRUE.
#' @param reverse whether to reverse the color palette. Defaults to FALSE.
#' @export
#' @examples
#' scale_fill_jll()

scale_fill_jll <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jll_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' @title Modernise the look of a ggplot object
#'
#' @description This function applies a modern style to a ggplot object.
#' @export
#' @examples
#' theme_jll_modern()

theme_jll_modern <- function(...) {
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, face = "bold",
                                       color = "#222222"),
    plot.subtitle = ggplot2::element_text(size = 12,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_blank(),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(size = 12,
                                         color = "#222222"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 12,
                                        color = "#222222"),
    axis.title = ggplot2::element_text(size = 12,
                                       color = "#222222"),
    axis.text = ggplot2::element_text(size = 12,
                                      color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,
                                                                 b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 0.5, color = "#222222"),
    axis.ticks.x = ggplot2::element_line(size = 0.5, color = "#222222"),
    axis.ticks.length.x = grid::unit(0.3, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 11, hjust = 0),
    strip.text.x = ggplot2::element_text(hjust = 0.5))
}

#' @title Modernise the look of a ggplot object
#'
#' @description This function applies a modern dark-theme style to a ggplot object.
#' @export
#' @examples
#' theme_jll_modern_dark()

theme_jll_modern_dark <- function(...) {
  font <- "Source Sans Pro"
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, face = "bold",
                                       color = "#D8D8D9"),
    plot.subtitle = ggplot2::element_text(
                                          size = 12,
                                          color = "#D8D8D9",
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_text(size = 10,
                                         color = "#D8D8D9"),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(size = 12,
                                         color = "#D8D8D9"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 12,
                                        color = "#D8D8D9"),
    axis.title = ggplot2::element_text(size = 12,
                                       color = "#D8D8D9"),
    axis.text = ggplot2::element_text(size = 12,
                                      color = "#D8D8D9"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,
                                                                 b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 0.5, color = "#D8D8D9"),
    axis.ticks.x = ggplot2::element_line(size = 0.5, color = "#D8D8D9"),
    axis.ticks.length.x = grid::unit(0.3, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#898B8E"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "#0E0E16", colour = NA),
    plot.background = ggplot2::element_rect(fill = "#0E0E16", colour = NA),
    strip.background = ggplot2::element_rect(fill = "#0E0E16"),
    strip.text = ggplot2::element_text(color = "#D8D8D9", size = 11, hjust = 0),
    strip.text.x = ggplot2::element_text(hjust = 0.5))
}

#' @title Style a map with a minimal look
#'
#' @description This function applies a clean theme to a map. It removes all grids and axis markings.
#' @export
#' @examples
#' theme_map()

theme_map <- function(...) {
  theme(
    # remove all axes
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # add a subtle grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # background colors
    plot.background = element_rect(fill = "white",
                                   color = NA),
    panel.background = element_rect(fill = "white",
                                    color = NA),
    legend.background = element_rect(fill = "white",
                                     color = NA),
    text = element_text(color = "black"),
    # titles
    legend.position = "bottom",
    legend.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 10),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    # captions
    plot.caption = element_text(size = 7,
                                color = "black"),
    ...
  )
}

#' @title Style a map with a minimal look
#'
#' @description This function applies a clean dark-theme to a map. It removes all grids and axis markings.
#' @export
#' @examples
#' theme_map_dark()

theme_map_dark <- function(...) {
  theme(
    # remove all axes
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # add a subtle grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # background colors
    plot.background = element_rect(fill = "#0E0E16",
                                   color = NA),
    panel.background = element_rect(fill = "#0E0E16",
                                    color = NA),
    legend.background = element_rect(fill = "#0E0E16",
                                     color = NA),
    text = element_text(color = "#EBECEC"),
    # borders and margins
    # titles
    legend.position = "bottom",
    legend.title = element_text(size = 11, color = "#EBECEC"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 10),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    # captions
    plot.caption = element_text(size = 7,
                                color = "#EBECEC"),
    ...
  )
}

