return_rhombus <- function(point_x, point_y, rotation, side_length, shape) {
  if(shape == 't') {

    triangle_height <- sin(72 * pi/180) * side_length

    rhombus <- data.frame(triangle = c(1, 2),
                          shape = c("t", "t"),
                          point1_x = point_x + side_length *
                            cos((rotation - 18) * pi/180),
                          point1_y = point_y + side_length *
                            sin((rotation - 18) * pi/180),
                          point2_x = point_x + c(0, 2 * triangle_height *
                                                   cos(rotation * pi/180)),
                          point2_y = point_y + c(0, 2 * triangle_height *
                                                   sin(rotation * pi/180)),
                          point3_x = point_x + side_length *
                            cos((rotation + 18) * pi/180),
                          point3_y = point_y + side_length *
                            sin((rotation + 18) * pi/180),
                          rhombus = 1)
  } else {
    triangle_base <- sqrt(2 * side_length^2 -
                            2 * side_length^2 * cos(108 * pi/180))

    rhombus <- data.frame(triangle = c(1, 2),
                          shape = c("T", "T"),
                          point1_x = point_x,
                          point1_y = point_y,
                          point2_x = c(point_x + side_length *
                                         cos((rotation - 36) * pi/180),
                                       point_x + side_length *
                                         cos((rotation + 36) * pi/180)),
                          point2_y = c(point_y + side_length *
                                         sin((rotation - 36) * pi/180),
                                       point_y + side_length *
                                         sin((rotation + 36) * pi/180)),
                          point3_x = point_x +
                            triangle_base * cos(rotation * pi/180),
                          point3_y = point_y +
                            triangle_base * sin(rotation * pi/180),
                          rhombus = 1)
  }

  return(rhombus)
}
