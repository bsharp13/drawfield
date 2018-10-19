#' Draw a soccer field
#'
#' Takes width, length, and color arguments to create a soccer field
#'
#' This function creates a plot that acts as a base layer on which various
#' points and lines can be plotted.
#'
#' @author Braden Sharp
#'
#' @param length     the length (x-axis) of the field
#' @param width      the width (y-axis) of the field
#' @param main the title that appears at the top of the plot
#' @param col1    the color of the field itself
#' @param col2    the color of the boundary lines on the field
#'
#' @import plotrix
#' @importFrom graphics lines plot points polygon segments text
#' @return Plots a soccer field
#' @export
#'
#' @examples
#' draw.soccer()

draw.soccer <- function(length = 120, width = 70, main = "Soccer",
                        col1 = "palegreen3", col2 = "white") {

  # Create dataframe of polygon coordinates
  wid.half <- width / 2
  len.half <- length / 2

  coords <- data.frame(
    Polygon = c('Boundary', 'L18', 'R18', 'L6', 'R6'),
    X1 = c(0, 0, length, 0, length),
    X2 = c(length, 18, length - 18, 6, length - 6),
    Y1 = c(0, wid.half - 22, wid.half - 22, wid.half - 10, wid.half - 10),
    Y2 = c(width, wid.half + 22, wid.half + 22, wid.half + 10, wid.half + 10)
  )

  # Create dataframe of arc coordinates
  l.pen.angle <- (asin(4 / 5) * 180) / pi
  r.pen.angle1 <- ((pi / 2 + acos(4 / 5)) * 180) / pi
  r.pen.angle2 <- ((pi + asin(4 / 5)) * 180) / pi

  arc.coords <- data.frame(
    Arc = c('L18', 'R18', 'BottomL', 'TopL', 'TopR', 'BottomR'),
    X = c(12, length - 12, 0, 0, length, length),
    Y = c(wid.half, wid.half, 0, width, width, 0),
    R = c(rep(10, 2), rep(2, 4)),
    Deg1 = c(l.pen.angle, r.pen.angle1, 90, 0, 180, 90),
    Deg2 = c(-l.pen.angle, r.pen.angle2, 0, -90, 270, 180)
  )

  # Create plotting boundary
  plot(
    NA,
    xlim = c(-1, length + 1),
    ylim = c(-1, width + 1),
    main = main,
    ylab = "",
    xlab = "",
    xaxt = "n",
    yaxt = "n"
  )

  # Draw blank field
  polygon(
    x = c(-1, -1, length + 1, length + 1),
    y = c(-1, width + 1, width + 1, -1),
    lwd = 2,
    col = col1,
    border = col1
  )

  # Draw other boxes
  sapply(1:nrow(coords), function(i) {
    polygon(
      x = c(coords$X1[i], coords$X1[i], coords$X2[i], coords$X2[i]),
      y = c(coords$Y1[i], coords$Y2[i], coords$Y2[i], coords$Y1[i]),
      lwd = 2,
      col = col1,
      border = col2
    )
  })

  # Draw penalty spots
  points(c(12, length - 12), rep(wid.half, 2), pch = 20, col = col2)

  # Draw arcs for penalty boxes and corners
  sapply(1:nrow(arc.coords), function(i) {
    draw.arc(
      x = arc.coords$X[i],
      y = arc.coords$Y[i],
      radius = arc.coords$R[i],
      deg1 = arc.coords$Deg1[i],
      deg2 = arc.coords$Deg2[i],
      lwd = 2,
      col = col2
    )
  })

  # Draw midfield line and circle
  segments(
    x0 = len.half, y0 = 0, x1 = len.half, y1 = width, lwd = 2, col = col2
  )
  draw.circle(x = len.half, y = wid.half, radius = 10, lwd = 2, border = col2)

  # Draw goal lines
  lines(
    x = c(0, 0), y = c(wid.half - 4.25, wid.half + 4.25), lwd = 5, col = col2
  )
  lines(
    x = c(length, length),
    y = c(wid.half - 4.25, wid.half + 4.25),
    lwd = 5,
    col = col2
  )
}
