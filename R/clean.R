#' Cleans out unseen triangles
#'
#' This function removes triangles that won't appear in a specified rectangle.
#' The purpose is to reduce the number of triangles to subdivide if they won't be
#' seen in the final output. If a triangle is fully above, below, or to the side
#' of the rectangle defined by input parameters, then it is removed from the data set.
#' The parameters \code{lower_x} with \code{lower_y} are the bottom
#' left corner and \code{upper_x} with \code{upper_y} are the top right corner.
#' Please not this might not remove all triangles that won't be seen as some can
#' extend across the diagonals and be missed by this filtering.
#'
#' @param df A dataset of triangles
#' @param lower_x x position of the bottom left corner
#' @param lower_y y position of the bottom left corner
#' @param upper_x x position of the top right corner
#' @param upper_y y position of the top right corner
#'
#' @return A data set of filtered triangles
#' @export
#'
#' @examples
#' return_starting_circle(0, 0, 0, 5, "t") %>% clean(-3, -3, -.5, -.5)
#' @importFrom rlang .data
clean <- function(df, lower_x, lower_y, upper_x, upper_y) {
  df <- df %>%
    dplyr::filter(!((.data$point1_x < lower_x &
      .data$point2_x < lower_x &
      .data$point3_x < lower_x) |
      (.data$point1_x > upper_x &
        .data$point2_x > upper_x &
        .data$point3_x > upper_x) |
      (.data$point1_y < lower_y &
        .data$point2_y < lower_y &
        .data$point3_y < lower_y) |
      (.data$point1_y > upper_y &
        .data$point2_y > upper_y &
        .data$point3_y > upper_y)))
}
