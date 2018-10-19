#' Draw a volleyball court
#'
#' Takes width, length, and color arguments to create a volleyball court
#'
#' This function creates a plot that acts as a base layer on which various
#' points and lines can be plotted.
#'
#' @author Braden Sharp
#'
#' @param length     the length (x-axis) of the court
#' @param width      the width (y-axis) of the court
#' @param main the title that appears at the top of the plot
#' @param col1    the color of the court itself
#' @param col2    the color of the lines on the court
#'
#' @importFrom graphics lines plot points polygon segments text
#' @return Plots a volleyball court
#'
#' @examples
#' draw.basketball()

draw.basketball <- function(length = 94, width = 50, main = "BYU Basketball",
                            col1 = "burlywood2", col2 = "darkblue") {
  lwd <- 2

  # Create dataframe of polygon coordinates
  wid.half <- width / 2
  len.half <- length / 2

  # coords <- data.frame(
  #   Polygon = c('Court', 'Boundary', 'LeftKey', 'RightKey'),
  #   X1 = c(-3, 0, 0, length - 18),
  #   X2 = c(length + 3, length, 18, length),
  #   Y1 = c(-3, 0, wid.half - 6, wid.half - 6),
  #   Y2 = c(width + 3, width, wid.half + 6, wid.half + 6),
  #   Color = c(rep(col1, 4)),
  #   Border = c(col1, rep(col2, 3)),
  #   stringsAsFactors = FALSE
  # )

  coords <- data.frame(
    Polygon = c('Court', 'Boundary', 'LeftKey', 'RightKey'),
    X1 = c(-3, 0),
    X2 = c(length + 3, length),
    Y1 = c(-3, 0),
    Y2 = c(width + 3, width),
    Color = c(col1, col1),
    Border = c(col1, col2),
    stringsAsFactors = FALSE
  )

  # Create dataframe of segment coordinates
  seg.coords <- data.frame(
    X0 = c(
      len.half, 0, 0, length, length, 0, 0, 18, rep(length - 18, 3),
      0, 0, rep(length - 18, 2)),
    X1 = c(
      len.half, 12, 12, rep(length - 12, 2),
      rep(18, 3), rep(length, 2), length - 18,
      18, 18, length, length
    ),
    Y0 = c(
      0, 4.5, width - 4.5, 4.5, width - 4.5, wid.half - 6, wid.half + 6,
      wid.half - 7, wid.half - 6, wid.half + 6, wid.half - 7,
      wid.half - 7, wid.half + 7, wid.half - 7, wid.half + 7
    ),
    Y1 = c(
      width, 4.5, width - 4.5, 4.5, width - 4.5, wid.half - 6, wid.half + 6,
      wid.half + 7, wid.half - 6, wid.half + 6, wid.half + 7,
      wid.half - 7, wid.half + 7, wid.half - 7, wid.half + 7
    )
  )

  # degree1 <- pi/2
  # draw.arc( x = 18, y = width/2, radius = (width-0.7)/6.00, angle1 = degree1, angle2 = -degree1, lwd = lwd, col = col2)
  # draw.arc( x = length - 18, y = width/2, radius = (width-0.7)/6.00, angle1 = pi + degree1, angle2 = pi - degree1, lwd = lwd, col = col2)
  #
  #
  # degree2 <- 0.97
  # draw.arc(4.9, width/2, (length/2)-16.75, angle1 = asin(degree2), angle2 = -asin(degree2), lwd = lwd, col = col2) # Left three point
  # draw.arc(length-4.9, width/2, (length/2)-16.75, angle1 = pi + asin(degree2), angle2 = pi - asin(degree2), lwd = lwd, col = col2) # Right three

  # Create plotting boundary
  plot(
    NA,
    xlim = c(-3, length + 3),
    ylim = c(-3, width + 3),
    main = main,
    ylab = "",
    xlab = "",
    xaxt = "n",
    yaxt = "n"
  )

  # Draw polygons
  invisible(sapply(1:nrow(coords), function(i) {
    polygon(
      x = c(coords$X1[i], coords$X1[i], coords$X2[i], coords$X2[i]),
      y = c(coords$Y1[i], coords$Y2[i], coords$Y2[i], coords$Y1[i]),
      lwd = 2,
      col = coords$Color[i],
      border = coords$Border[i]
    )
  }))

  # Draw key circles and three-point lines
  invisible(sapply(c(18, length - 18), function(i) {
    draw.circle(
      x = i,
      y = wid.half,
      radius = 6,
      border = col2,
      lwd = 2
    )
  }))

  # Draw segments
  invisible(sapply(1:nrow(seg.coords), function(i) {
    segments(
      x0 = seg.coords$X0[i],
      x1 = seg.coords$X1[i],
      y0 = seg.coords$Y0[i],
      y1 = seg.coords$Y1[i],
      col = col2,
      lwd = lwd
    )
  }))

}

draw.basketball()

draw.ellipse(
  x=25,
  y=5.25,
  deg=TRUE,
  angle=0,
  arc.only= TRUE,
  segment=rbind(c(22,0),c(158,360)),
  a=23.75,
  b=23.75,
  border=1,
  nv=200,
  lwd=3)

three.point.start <- 8
l.angle <- 79.5
draw.ellipse(
  x = three.point.start,
  y = wid.half,
  a = 29 - three.point.start,
  b = 29 - three.point.start,
  segment = rbind(c(l.angle,0),c(-l.angle, 0)),

  border = col2,
  lwd = 2)
segments(x0 = 11, y0 = 4.5, x1 = 12, y1 = width - 4.5, col = col1, lwd = 3)
