#' 'Flexoki' Color Scales for `ggplot2`
#'
#' @param palette palette from `names(flexoki)` to use
#' @param which numeric indices of colors to use. `NULL` by default.
#' @param ... arguments passed on to `ggplot2::discrete_scale()`
#' @param reverse Should the vector be reversed? Default is `FALSE`.
#'
#' @return A [ggplot2::Scale]
#'
#' @examples
#' ggplot2::mpg |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_point(ggplot2::aes(displ, hwy, colour = class)) +
#'   scale_color_flexoki(palette = 'dark')
#'
#' ggplot2::mpg |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_point(ggplot2::aes(displ, hwy, fill = class),
#'     pch = 23, color = 'transparent'
#'   ) +
#'   scale_fill_flexoki(palette = 'light')
#'
#' @rdname scale_flexoki
#' @export
scale_color_flexoki <- function(palette = 'dark', which = NULL, ...,
                                reverse = FALSE) {
  pal <- flexoki[[palette]]
  if (!is.null(which)) {
    pal <- pal[which]
  }
  if (reverse) {
    pal <- rev(pal)
  }
  ggplot2::discrete_scale(
    aesthetics = 'color', scale_name = palette,
    palette = palette::palette_function(pal), ...
  )
}

#' @rdname scale_flexoki
#' @export
scale_fill_flexoki <- function(palette = 'dark', which = NULL, ...,
                               reverse = FALSE) {
  pal <- flexoki[[palette]]
  if (!is.null(which)) {
    pal <- pal[which]
  }
  if (reverse) {
    pal <- rev(pal)
  }
  ggplot2::discrete_scale(
    aesthetics = 'fill', scale_name = palette, ...,
    palette = palette::palette_function(pal)
  )
}

#' @rdname scale_flexoki
#' @export
scale_colour_flexoki <- scale_color_flexoki
