#' Light Theme
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional parameters
#'
#' @return ggplot2 theme
#' @export
#' @concept theme
#'
#' @examples
#' ggplot2::mpg |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_point(ggplot2::aes(displ, hwy, colour = class)) +
#'   scale_color_flexoki_d(palette = 'light') +
#'   theme_flexoki_light()
#'
theme_flexoki_light <- function(base_size = 12, base_family = 'mono', ...) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = flexoki$base['paper'], color = NA),
      panel.background = ggplot2::element_rect(fill = flexoki$base['paper'], color = NA),
      panel.grid.major = ggplot2::element_line(color = flexoki$base['base100']),
      panel.grid.minor = ggplot2::element_line(color = flexoki$base['base50']),
      axis.text = ggplot2::element_text(color = flexoki$base['base700']),
      axis.title = ggplot2::element_text(color = flexoki$base['base900']),
      axis.ticks = ggplot2::element_line(color = flexoki$base['base400']),
      plot.title = ggplot2::element_text(color = flexoki$base['base900'], face = 'bold', size = base_size * 1.2),
      plot.subtitle = ggplot2::element_text(color = flexoki$base['base600'], size = base_size),
      plot.caption = ggplot2::element_text(color = flexoki$base['base500'], size = base_size * 0.8),
      legend.background = ggplot2::element_rect(fill = flexoki$base['paper'], color = NA),
      legend.key = ggplot2::element_rect(fill = flexoki$base['paper'], color = NA),
      legend.text = ggplot2::element_text(color = flexoki$base['base900']),
      legend.title = ggplot2::element_text(color = flexoki$base['base900']),
      ...
    )
}

#' Dark Theme
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional parameters
#'
#' @return ggplot2 theme
#' @export
#' @concept theme
#'
#' @examples
#' ggplot2::mpg |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_point(ggplot2::aes(displ, hwy, colour = class)) +
#'   scale_color_flexoki_d(palette = 'dark') +
#'   theme_flexoki_dark()
#'
theme_flexoki_dark <- function(base_size = 12, base_family = 'mono', ...) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = flexoki$base['black'], color = NA),
      panel.background = ggplot2::element_rect(fill = flexoki$base['black'], color = NA),
      panel.grid.major = ggplot2::element_line(color = flexoki$base['base900']),
      panel.grid.minor = ggplot2::element_line(color = flexoki$base['base950']),
      axis.text = ggplot2::element_text(color = flexoki$base['base500']),
      axis.title = ggplot2::element_text(color = flexoki$base['base50']),
      axis.ticks = ggplot2::element_line(color = flexoki$base['base600']),
      plot.title = ggplot2::element_text(color = flexoki$base['base50'], face = 'bold', size = base_size * 1.2),
      plot.subtitle = ggplot2::element_text(color = flexoki$base['base500'], size = base_size),
      plot.caption = ggplot2::element_text(color = flexoki$base['base500'], size = base_size * 0.8),
      legend.background = ggplot2::element_rect(fill = flexoki$base['black'], color = NA),
      legend.key = ggplot2::element_rect(fill = flexoki$base['black'], color = NA),
      legend.text = ggplot2::element_text(color = flexoki$base['base50']),
      legend.title = ggplot2::element_text(color = flexoki$base['base50']),
      ...
    )
}
