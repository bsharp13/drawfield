#' Draw a tennis court
#'
#' Takes width, length, and color arguments to create a tennis court
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
#' @return Plots a tennis court
#' @export
#'
#' @examples
#' draw.tennis()

draw.tennis <- function(length = 78, width = 36, main = "Tennis",
                        col1 = "aquamarine3", col2 = "white") {

  len.half <- length / 2
  wid.half <- width / 2

  # Create data frame of polygon definitions
  coords <- data.frame(
    X1 = c(-7, 0),
    X2 = c(length + 7, length),
    Y1 = c(-5, 0),
    Y2 = c(width + 5, width),
    Color = rep(col1, 2),
    Border = c(col1, col2),
    stringsAsFactors = FALSE
  )

  # Create data frame of segment definitions
  seg.coords <- data.frame(
    X0 = c(len.half, 18, length - 18, 0, 0, 18, len.half),
    X1 = c(len.half, 18, length - 18, length, length, len.half, length - 18),
    Y0 = c(0, rep(4.5, 3), width - 4.5, wid.half, wid.half),
    Y1 = c(width, width - 4.5, width - 4.5, 4.5, width - 4.5, rep(wid.half, 2))
  )

  # Define plot boundaries
  plot(
    NA,
    xlim = c(-7, length + 7),
    ylim = c(-5, width + 5),
    main = main,
    ylab = "",
    xlab = "",
    xaxt = "n",
    yaxt = "n"
  )

  # Draw court and court boundaries
  invisible(sapply(1:nrow(coords), function(i) {
    polygon(
      x = c(coords$X1[i], coords$X1[i], coords$X2[i], coords$X2[i]),
      y = c(coords$Y1[i], coords$Y2[i], coords$Y2[i], coords$Y1[i]),
      lwd = 2,
      col = coords$Color[i],
      border = coords$Border[i]
    )
  }))

  # Draw line segments
  invisible(sapply(1:nrow(seg.coords), function(i) {
    segments(
      x0 = seg.coords$X0[i],
      y0 = seg.coords$Y0[i],
      x1 = seg.coords$X1[i],
      y1 = seg.coords$Y1[i],
      col = col2,
      lwd = 2
    )
  }))
}
