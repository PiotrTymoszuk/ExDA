# `ggplot` graphic utlities

# Themes -----------

#' Custom `ggplot` themes.
#'
#' @description
#' Themes compatible with requirements of most of scientific journals.
#'
#' @return `ggplot`'s `theme` objects.
#'
#' @param font_size font size used in the theme. The default is 8.
#'
#' @export

  eda_classic_theme <- function(font_size = 8) {

    common_text <- element_text(size = font_size,
                                face = "plain",
                                color = "black")

    theme_classic() +
      theme(axis.text = common_text,
            axis.title = common_text,
            plot.title = element_text(size = font_size,
                                      face = "bold"),
            plot.subtitle = common_text,
            plot.tag = element_text(size = font_size,
                                    face = "plain",
                                    color = "black",
                                    hjust = 0,
                                    vjust = 1),
            plot.tag.position = "bottom",
            legend.text = common_text,
            legend.title = common_text,
            strip.text = common_text,
            strip.background = element_rect(fill = "gray95",
                                            color = "gray80"),
            plot.margin = margin(t = 5,
                                 l = 4,
                                 r = 2,
                                 unit = "mm"),
            panel.grid.major = element_line(color = "gray90"))

  }

#' @rdname eda_classic_theme
#' @export

  eda_void_theme <- function(font_size = 8) {

    common_text <- element_text(size = font_size,
                                face = "plain",
                                color = "black")

    theme() +
      theme(plot.title = element_text(size = font_size,
                                      face = "bold"),
            plot.subtitle = common_text,
            legend.text = common_text,
            legend.title = common_text,
            plot.margin = margin(t = 5,
                                 l = 4,
                                 r = 2,
                                 unit = "mm"))

  }

# Color palettes ---------

#' Color palettes.
#'
#' @description
#' A medley of functions returning named character vectors with
#' Tableau and Wes Anderson colors.
#'
#' @param name logical should the vector be returned with the color names?
#'
#' @return named or unnamed character vectors with hex codes of colors.
#'
#' @export

  tableau20_colors <- function(name = FALSE) {

    cols <-
      c(
        "Dark_Blue" = "#1F77B4",
        "Light_Blue" = "#AEC7E8",
        "Dark_Orange" = "#FF7F0E",
        "Light_Orange" = "#FFBB78",
        "Dark_Green" = "#2CA02C",
        "Light_Green" = "#98DF8A",
        "Dark_Red" = "#D62728",
        "Light_Red" = "#FF9896",
        "Dark_Purple" = "#9467BD",
        "Light_Purple" = "#C5B0D5",
        "Dark_Brown" = "#8C564B",
        "Light_Brown" = "#C49C94",
        "Dark_Pink" = "#E377C2",
        "Light_Pink" = "#F7B6D2",
        "Dark_Gray" = "#7F7F7F",
        "Light_Gray" = "#C7C7C7",
        "Dark_Yellow-Green" = "#BCBD22",
        "Light_Yellow-Green" = "#DBDB8D",
        "Dark_Teal" = "#17BECF",
        "Light_Teal" = "#9EDAE5"
      )

    if(name) return(cols) else return(unname(cols))

  }

#' @rdname tableau20_colors
#' @export

  tableau10_colors <- function(name = FALSE) {

    cols <-
      c(
        "Blue" = "#4E79A7",
        "Orange" = "#F28E2B",
        "Red" = "#E15759",
        "Teal" = "#76B7B2",
        "Green" = "#59A14F",
        "Yellow" = "#EDC949",
        "Purple" = "#AF7AA1",
        "Pink" = "#FF9DA7",
        "Brown" = "#9C755F",
        "Gray" = "#BAB0AB"
      )

    if(name) return(cols) else return(unname(cols))

  }

#' @rdname tableau20_colors
#' @export

  wes_grandbudapest1_colors <- function(name = FALSE) {

    cols <-
      c(
        "Salmon" = "#F1BB7B",
        "Red" = "#FD6467",
        "Brown" = "#5B1A18",
        "Orange" = "#D67236"
      )

    if(name) return(cols) else return(unname(cols))

  }

#' @rdname tableau20_colors
#' @export

  wes_grandbudapest2_colors <- function(name = FALSE) {

    cols <-
      c(
        "Light_Pink" = "#E6A0C4",
        "Light_Blue" = "#C6CDF7",
        "Tan" = "#D8A499",
        "Dark_Blue" = "#7294D4"
      )

    if(name) return(cols) else return(unname(cols))

  }

#' @rdname tableau20_colors
#' @export

  wes_zissou1_colors <- function(name = FALSE) {

    cols <-
      c(
        "Light_Blue" = "#3B9AB2",
        "Medium_Blue" = "#78B7C5",
        "Yellow" = "#EBCC2A",
        "Dark_Yellow" = "#E1AF00",
        "Orange_Red" = "#F21A00"
      )

    if(name) return(cols) else return(unname(cols))

  }

#' @rdname tableau20_colors
#' @export

  wes_fantasticfox1_colors <- function(name = FALSE) {

    cols <-
      c(
        "Orange" = "#DD8D29",
        "Yellow" = "#E2D200",
        "Teal" = "#46ACC8",
        "Burnt_Orange" = "#E58601",
        "Brown" = "#B40F20"
      )

    if(name) return(cols) else return(unname(cols))

  }

#' @rdname tableau20_colors
#' @export

  wes_moonrise1_colors <- function(name = FALSE) {

    cols <-
      c(
        "Yellow" = "#F3DF6C",
        "Dark_Yellow" = "#CEAB07",
        "Light_Gray" = "#D5D5D3",
        "Dark_Gray" = "#24281A"
      )

    if(name) return(cols) else return(unname(cols))

  }

# END ---------
