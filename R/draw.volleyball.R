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
#' @export
#'
#' @examples
#' draw.volleyball()

draw.volleyball <- function( length = 18, width = 9, main = "Volleyball",
                             col1 = "burlywood2", col2 = "darkblue") {

  len.half <- length / 2
  wid.half <- width / 2

  # Create data frame of polygon definitions
  coords <- data.frame(
    X1 = c(-1, 0),
    X2 = c(length + 1, length),
    Y1 = c(-1, 0),
    Y2 = c(width + 1, width),
    Color = rep(col1, 2),
    Border = c(col1, col2),
    stringsAsFactors = FALSE
  )


  # Create data frame of segment definitions
  seg.coords <- data.frame(
    X0 = c(len.half, len.half + 3, len.half - 3),
    Y0 = rep(0, 3),
    Y1 = rep(width, 3),
    Color = c('white', rep(col2, 2)),
    Width = c(4, 3, 3),
    stringsAsFactors = FALSE
  )

  # Define plot boundaries
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

  # Draw court and court boundaries
  sapply(1:nrow(coords), function(i) {
    polygon(
      x = c(coords$X1[i], coords$X1[i], coords$X2[i], coords$X2[i]),
      y = c(coords$Y1[i], coords$Y2[i], coords$Y2[i], coords$Y1[i]),
      col = coords$Color[i],
      border = coords$Border[i],
      lwd = 3
    )
  })

  # Draw line segments
  invisible(sapply(1:nrow(seg.coords), function(i) {
    segments(
      x0 = seg.coords$X0[i],
      x1 = seg.coords$X0[i],
      y0 = seg.coords$Y0[i],
      y1 = seg.coords$Y1[i],
      col = seg.coords$Color[i],
      lwd = seg.coords$Width[i]
    )
  }))
}
