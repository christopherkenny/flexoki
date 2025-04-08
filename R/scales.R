#' Discrete 'Flexoki' Color Scales for `ggplot2`
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
#'   scale_color_flexoki_d(palette = 'dark')
#'
#' ggplot2::mpg |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_point(ggplot2::aes(displ, hwy, fill = class),
#'     pch = 23, color = 'transparent'
#'   ) +
#'   scale_fill_flexoki_d(palette = 'light')
#'
#' @rdname scale_flexoki_d
#' @export
scale_color_flexoki_d <- function(palette = 'dark', which = NULL, ...,
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

#' @rdname scale_flexoki_d
#' @export
scale_fill_flexoki_d <- function(palette = 'dark', which = NULL, ...,
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

#' @rdname scale_flexoki_d
#' @export
scale_colour_flexoki_d <- scale_color_flexoki_d

#' Continuous 'Flexoki' Color Scales for `ggplot2`
#'
#' @param palette palette from `names(flexoki)` to use
#' @param ... arguments passed on to `ggplot2::discrete_scale()`
#' @param reverse Should the vector be reversed? Default is `FALSE`.
#'
#' @return A [ggplot2::Scale]
#'
#' @examples
#' ggplot2::faithfuld |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_tile(ggplot2::aes(waiting, eruptions, fill = density)) +
#'   scale_fill_flexoki_c(palette = 'red')
#'
#' @rdname scale_flexoki_c
#' @export
scale_color_flexoki_c <- function(palette = 'red', ...,
                                 reverse = FALSE) {
  pal <- flexoki[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  ggplot2::scale_colour_gradientn(..., colours = pal)
}

#' @rdname scale_flexoki_c
#' @export
scale_fill_flexoki_c <- function(palette = 'red', ...,
                                 reverse = FALSE) {
  pal <- flexoki[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  ggplot2::scale_fill_gradientn(..., colours = pal)
}

#' @rdname scale_flexoki_c
#' @export
scale_colour_flexoki_c <- scale_color_flexoki_c


#' Binned 'Flexoki' Color Scales for `ggplot2`
#'
#' @param palette palette from `names(flexoki)` to use
#' @param ... arguments passed on to `ggplot2::discrete_scale()`
#' @param reverse Should the vector be reversed? Default is `FALSE`.
#'
#' @return A [ggplot2::Scale]
#'
#' @examples
#' ggplot2::faithfuld |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_tile(ggplot2::aes(waiting, eruptions, fill = density)) +
#'   scale_fill_flexoki_b(palette = 'red')
#'
#' @rdname scale_flexoki_b
#' @export
scale_color_flexoki_b <- function(palette = 'red', ...,
                                  reverse = FALSE) {
  pal <- flexoki[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  ramp <- scales::colour_ramp(pal)
  ggplot2::binned_scale('color', palette, palette = ramp, ...)
}

#' @rdname scale_flexoki_b
#' @export
scale_fill_flexoki_b <- function(palette = 'red', ...,
                                 reverse = FALSE) {
  pal <- flexoki[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  ramp <- scales::colour_ramp(pal)
  ggplot2::binned_scale('fill', palette, palette = ramp, ...)
}

#' @rdname scale_flexoki_b
#' @export
scale_colour_flexoki_b <- scale_color_flexoki_b
