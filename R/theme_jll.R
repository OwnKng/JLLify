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

#' @title Create a base ggtern object to plot over
#'
#' @description This function creates a ternary plot base using ggtern. It creates the plot and creates three segments that easilly separate zones in the data, making the chart easier to interpret
#' @param x_color the color of the 'x' segment. Defaults to "#de8a5a"
#' @param y_color the color of the 'y' segment. Defaults to "#CFC994"
#' @param z_color the color of the 'z' segment. Defaults to "#008080"
#' @param x_label the name of the 'x' label. Defaults to "Low"
#' @param y_label the name of the 'y' label. Defaults to "Medium"
#' @param z_label the name of the 'z' label. Defaults to "High"
#' @export
#' @examples
#' ggtern_base()


ggtern_base <- function(x_color = "#de8a5a", y_color = "#CFC994", z_color = "#008080",
                        x_label = "Low", y_label = "Medium", z_label = "High"){

  # Polygons for the shading
  left <- tibble(x = c(0.5, 0.5, 1, 0.5),
                     y = c(0.5, 0, 0, 0.5),
                     z = c(0.5, 0.5, 0, 0))

  right <- tibble(x = c(0.5, 0.5, 0, 0),
                    y = c(0.5, 0, 0, 0.5),
                    z = c(0.5, 0.5, 1, 0.5))

  top <- tibble(x = c(0.5, 0.5, 0),
                     y = c(0.5, 0.5, 1),
                     z = c(0.5, 0, 0))

  top_2 <- tibble(x = c(0.5, 0, 0),
                       y = c(0.5, 0.5, 1),
                       z = c(0.5, 0.5, 0))

  # Create the base graph
  suppressWarnings(
  ggtern_base <- ggtern() +
    annotate(geom = "text", label = y_label,
             x = c(0.15, 0.15, 0.15),
             y = c(0.15, 0.15, 0.15),
             z = c(0.7, 0.7, 0.7),
             size = 10,
             color = y_color,
             fontface = "bold") +
    annotate(geom = "text", label = x_label,
             x = c(0.7, 0.7, 0.7),
             y = c(0.15, 0.15, 0.15),
             z = c(0.15, 0.15, 0.15),
             size =  10,
             color = x_color,
             fontface = "bold") +
    annotate(geom = "text", label = z_label,
             x = c(0.15, 0.15, 0.15),
             y = c(0.7, 0.7, 0.7),
             z = c(0.15, 0.15, 0.15),
             size = 10,
             color = z_color,
             fontface = "bold") +
    geom_polygon(data = top, aes(x, y, z),
                 fill = z_color,
                 alpha = 0.6) +
    geom_polygon(data = top_2, aes(x, y, z),
                 fill = z_color, alpha = 0.6) +
    geom_polygon(data = left,
                 aes(x, y, z),
                 fill = x_color,
                 col = "white",
                 size = 1,
                 alpha = 0.6) +
    geom_polygon(data = right, aes(x, y, z),
                 fill = y_color,
                 col = "white",
                 size = 1,
                 alpha = 0.6)
  )

  return(ggtern_base)

}

#' @title Style a the ternary plot
#'
#' @description This function applies a modern look to a ggtern object
#' @param x_color the color of the 'x' segment. Defaults to "#de8a5a"
#' @param y_color the color of the 'y' segment. Defaults to "#CFC994"
#' @param z_color the color of the 'z' segment. Defaults to "#008080"
#' @export
#' @examples
#' theme_ternary_dark()


theme_ternary_dark <- function(plot, x_color = "#de8a5a", y_color = "#CFC994", z_color = "#008080", ...) {
    theme(
    # Re-color the axis ticks
    tern.axis.ticks.minor = element_blank(),
    tern.axis.ticks.length.major = unit(1, "cm"),
    tern.axis.ticks.major = element_line(size = 4),

    # Re-color lines and text

    tern.axis.line.L = element_line(color = x_color, size = 2),
    tern.axis.text.L = element_text(color = x_color, size = 24),
    tern.axis.title.L = element_text(color = x_color, size = 24),
    tern.axis.ticks.major.L = element_line(color = x_color),
    tern.axis.line.R = element_line(color = y_color, size = 2),
    tern.axis.text.R = element_text(color = y_color, size = 24),
    tern.axis.title.R = element_text(color = y_color, size = 24),
    tern.axis.ticks.major.R = element_line(color = y_color),
    tern.axis.line.T = element_line(color = z_color, size = 2),
    tern.axis.text.T = element_text(color = z_color, size = 24),
    tern.axis.ticks.major.T = element_line(color = z_color),
    tern.axis.title.T = element_text(color = z_color, size = 24),

    # Re-color the plot background
    tern.plot.background = element_rect(fill = "#0E0E16", color = NA),
    tern.panel.background = element_rect(fill = "#0E0E16", color = NA),
    tern.panel.grid.major =  element_line(size = 0.5, color = "lightgrey"),
    tern.panel.grid.minor = element_line(size = 0.5, color = "lightgrey"),

    plot.background = element_rect(fill = "#0E0E16"),
    strip.background = element_rect(fill = "#0E0E16"),

    # Re-color the facet texts
    strip.text = element_text(color = "white", size = 24, face = "bold", hjust = 0),
    strip.text.x = element_text(color = "white", hjust = 0.5),
    ...
  )
}


