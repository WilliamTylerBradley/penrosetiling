#' Subdivides one triangle
#'
#' This function subdivides one triangle into either two or three more based on
#' the Penrose tiling rules.
#'
#' If the shape is "t", there will be two triangles. If the shape is "T" or
#' anything else, there will be three triangles. The \code{point2_x} and
#' \code{point2_y} parameters determine the middle of the triangle. Which side
#' \code{point1_x} and \code{point1_y} versus \code{point3_x} and
#' \code{point3_y} determines if it is a left or right triangle. This function
#' is mostly used internally.
#'
#' @param shape Either "t" or "T" for a thin or thick triangle
#' @param point1_x x position of the first point
#' @param point1_y y position of the first point
#' @param point2_x x position of the middle point
#' @param point2_y y position of the middle point
#' @param point3_x x position of the last point
#' @param point3_y y position of the last point
#'
#' @return A data set of triangles
#' @export
#'
subdivide <- function(shape,
                      point1_x, point1_y,
                      point2_x, point2_y,
                      point3_x, point3_y) {
  g_ratio <- (1 + sqrt(5)) / 2

  if (shape == "t") {
    df <- data.frame(
      shape = c("t", "T"),
      point1_x = c(
        point2_x + (point1_x - point2_x) / g_ratio,
        point3_x
      ),
      point1_y = c(
        point2_y + (point1_y - point2_y) / g_ratio,
        point3_y
      ),
      point2_x = c(
        point3_x,
        point2_x + (point1_x - point2_x) / g_ratio
      ),
      point2_y = c(
        point3_y,
        point2_y + (point1_y - point2_y) / g_ratio
      ),
      point3_x = c(
        point1_x,
        point2_x
      ),
      point3_y = c(
        point1_y,
        point2_y
      )
    )
  } else {
    df <- data.frame(
      shape = c("T", "t", "T"),
      point1_x = c(
        point1_x + (point3_x - point1_x) / g_ratio,
        point1_x + (point2_x - point1_x) / g_ratio,
        point3_x
      ),
      point1_y = c(
        point1_y + (point3_y - point1_y) / g_ratio,
        point1_y + (point2_y - point1_y) / g_ratio,
        point3_y
      ),
      point2_x = c(
        point1_x + (point2_x - point1_x) / g_ratio,
        point1_x + (point3_x - point1_x) / g_ratio,
        point1_x + (point3_x - point1_x) / g_ratio
      ),
      point2_y = c(
        point1_y + (point2_y - point1_y) / g_ratio,
        point1_y + (point3_y - point1_y) / g_ratio,
        point1_y + (point3_y - point1_y) / g_ratio
      ),
      point3_x = c(
        point1_x,
        point2_x,
        point2_x
      ),
      point3_y = c(
        point1_y,
        point2_y,
        point2_y
      )
    )
  }

  return(df)
}
