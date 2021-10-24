#' Pivots a wide data set (one triangle per row) into a long data set (one point
#' per row)
#'
#' Most of the functions in the \code{penrosetiling} package use triangles in a
#' wide format, meaning all the points are in one row. For graphing with
#' \code{ggplot2} and other data manipulation, it is better to have the data in
#' long format, meaning one point per row with a column labeling the point
#' order. This function convert a wide data set to a long one. It serves mostly
#' as a small utility function.
#'
#' @param df Data set of triangles in wide format
#'
#' @return Data set of triangles in long format
#' @export
#'
#' @examples return_starting_circle(0, 0, 0, 5, "t") %>% pivot_down()
pivot_down <- function(df) {
  spec <- tibble::tribble(
    ~.name, ~.value, ~point,
    "point1_x", "x", 1,
    "point2_x", "x", 2,
    "point3_x", "x", 3,
    "point1_y", "y", 1,
    "point2_y", "y", 2,
    "point3_y", "y", 3
  )

  df <- tidyr::pivot_longer_spec(df, spec)
}
