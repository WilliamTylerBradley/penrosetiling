#' Get a starting rhombus
#'
#' This function returns a data set of two observations. Each observation is a
#' triangle for one half of a rhombus. The location, direction, and shape of the
#' rhombus is determined through the parameters.
#'
#' The starting point of rhombus is given by the \code{point_x} and
#' \code{point_y} parameters. This will appear in the data set as
#' \code{point1_x} and \code{point1_y}. The \code{rotation} in degrees will turn
#' the rhombus around this point while \code{side_length} will change the size.
#' Finally, \code{shape} will provide either a thick or thin rhombus. A "t" for
#' the shape parameter will yield a thin rhombus. Anything else will provide a
#' thick one, but traditionally a "T" is used.
#'
#' @param point_x Starting x position for first point
#' @param point_y Starting y position for first point
#' @param rotation Rotation about the starting position in degrees
#' @param side_length Length of one outside edge
#' @param shape Ether "t" for thin rhombus or "T" for thick rhombus
#'
#' @return Data set of triangles that join to create a rhombus
#' @export
#'
#' @examples
#' return_rhombus(0, 0, 0, 10, "T")
#' return_rhombus(1, 1, 90, .5, "t")
return_rhombus <- function(point_x, point_y, rotation, side_length, shape) {
  if (shape == "t") {
    triangle_height <- sin(72 * pi / 180) * side_length

    rhombus <- data.frame(
      triangle = c(1, 2),
      shape = c("t", "t"),
      point1_x = point_x + side_length *
        cos((rotation - 18) * pi / 180),
      point1_y = point_y + side_length *
        sin((rotation - 18) * pi / 180),
      point2_x = point_x + c(0, 2 * triangle_height *
        cos(rotation * pi / 180)),
      point2_y = point_y + c(0, 2 * triangle_height *
        sin(rotation * pi / 180)),
      point3_x = point_x + side_length *
        cos((rotation + 18) * pi / 180),
      point3_y = point_y + side_length *
        sin((rotation + 18) * pi / 180),
      rhombus = 1
    )
  } else {
    triangle_base <- sqrt(2 * side_length^2 -
      2 * side_length^2 * cos(108 * pi / 180))

    rhombus <- data.frame(
      triangle = c(1, 2),
      shape = c("T", "T"),
      point1_x = point_x,
      point1_y = point_y,
      point2_x = c(
        point_x + side_length *
          cos((rotation - 36) * pi / 180),
        point_x + side_length *
          cos((rotation + 36) * pi / 180)
      ),
      point2_y = c(
        point_y + side_length *
          sin((rotation - 36) * pi / 180),
        point_y + side_length *
          sin((rotation + 36) * pi / 180)
      ),
      point3_x = point_x +
        triangle_base * cos(rotation * pi / 180),
      point3_y = point_y +
        triangle_base * sin(rotation * pi / 180),
      rhombus = 1
    )
  }

  return(rhombus)
}
