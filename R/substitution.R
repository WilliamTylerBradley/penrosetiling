#' Apply substitution rules to a data set of rhombi/triangles
#'
#' This function replaces loops through a data set of triangles, subdivides
#' them, then updates the triangle ids and rhombus ids.
#'
#' The data set has to have \code{shape}, \code{point1_x}, \code{point1_y},
#' \code{point2_x}, \code{point2_y}, \code{point3_x}, and \code{point3_y}. These
#' columns will be updated. In addition to those, any column titles
#' \code{triangle} or \code{rhombus} will be updated to serve as identification
#' columns.
#'
#' There is a section of this code that will snap the points to a grid in an
#' attempt to fix numerical stability issues. There is potential for this to
#' cause errors but almost always fixes more problems than it creates.
#'
#' @param df A data set of triangles
#'
#' @return A data set of subdivided triangles
#' @export
#'
#' @examples
#' return_rhombus(0, 0, -90 + 36, 1, "T") %>% substitution()
#' @importFrom rlang .data
substitution <- function(df) {
  if(nrow(df) == 0) {
    return(df)
  }

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(new_triangles = list(subdivide(
      .data$shape,
      .data$point1_x, .data$point1_y,
      .data$point2_x, .data$point2_y,
      .data$point3_x, .data$point3_y
    ))) %>%
    dplyr::select(-c(
      .data$shape,
      .data$point1_x, .data$point1_y,
      .data$point2_x, .data$point2_y,
      .data$point3_x, .data$point3_y,
      .data$triangle, .data$rhombus
    )) %>%
    tidyr::unnest(cols = c(.data$new_triangles)) %>%
    tibble::rowid_to_column("triangle") %>%
    dplyr::mutate(
      point1_x = round(.data$point1_x, 7),
      point1_y = round(.data$point1_y, 7),
      point2_x = round(.data$point2_x, 7),
      point2_y = round(.data$point2_y, 7),
      point3_x = round(.data$point3_x, 7),
      point3_y = round(.data$point3_y, 7)
    ) %>%
    dplyr::group_by(.data$point1_x, .data$point1_y, .data$point3_x, .data$point3_y) %>%
    dplyr::mutate(rhombus = dplyr::cur_group_id()) %>%
    dplyr::ungroup()
}
