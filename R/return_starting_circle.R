#' Get a starting circle of rhombi
#'
#' This function returns a data set of ten observations. Each observation is a
#' triangle for one half of a rhombus. The location, direction, and shape of the
#' rhombus is determined through the parameters.
#'
#' The center point of the circle is given by the \code{point_x} and
#' \code{point_y} parameters. This will appear in the data set as
#' \code{point1_x} and \code{point1_y}. The \code{rotation} in degrees will turn
#' the rhombi around this point while \code{side_length} will change the size.
#' Finally, \code{shape} will provide either a thick or thin rhombus. A "t" for
#' the shape parameter will yield a thin rhombus. Anything else will provide a
#' thick one, but traditionally a "T" is used. Thin triangles all point into the
#' circle (ten rhombi halfs) while thick triangles alternate pointing to each
#' other (five full rhombi).
#'
#' @param point_x Starting x position for first point
#' @param point_y Starting y position for first point
#' @param rotation Rotation about the starting position in degrees
#' @param side_length Length of one outside edge
#' @param shape Ether "t" for thin rhombus or "T" for thick rhombus
#'
#' @return Data set of triangles that join to create a circle of rhombi
#' @export
#'
#' @examples
#' return_starting_circle(0, 0, 0, 1, "T")
#' return_starting_circle(5, 5, 180, 3, "t")
return_starting_circle <- function(point_x, point_y, rotation, side_length, shape) {
  if (shape == "t") {
    rotations <- seq(0, 9) * 36 + rotation
    point_order_flips <- rep(c(1, -1), 5)

    circle <- data.frame(
      triangle = seq(1, 10),
      shape = "t",
      point1_x = point_x + side_length *
        cos((rotations - (point_order_flips * 18)) * pi / 180),
      point1_y = point_y + side_length *
        sin((rotations - (point_order_flips * 18)) * pi / 180),
      point2_x = point_x,
      point2_y = point_y,
      point3_x = point_x + side_length *
        cos((rotations + (point_order_flips * 18)) * pi / 180),
      point3_y = point_y + side_length *
        sin((rotations + (point_order_flips * 18)) * pi / 180),
      rhombus = seq(1, 10)
    )
  } else {
    rotations <- rep(seq(0, 4) * 36 * 2 + rotation, each = 2)
    point_order_flips <- rep(c(-1, 1), 5)

    triangle_base <- sqrt(2 * side_length^2 -
      2 * side_length^2 * cos(108 * pi / 180))

    circle <- data.frame(
      triangle = seq(1, 10),
      shape = "T",
      point1_x = point_x,
      point1_y = point_y,
      point2_x = point_x + side_length *
        cos((rotations + (point_order_flips * 36)) * pi / 180),
      point2_y = point_y + side_length *
        sin((rotations + (point_order_flips * 36)) * pi / 180),
      point3_x = point_x +
        triangle_base * cos(rotations * pi / 180),
      point3_y = point_y +
        triangle_base * sin(rotations * pi / 180),
      rhombus = rep(seq(1, 5), each = 2)
    )
  }

  return(circle)
}
