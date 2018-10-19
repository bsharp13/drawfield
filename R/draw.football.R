#' Draw an American football field
#'
#' Takes width, length, and color arguments to create a football field
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
#' @param col2    the color of the lines on the field
#' @param col3    the color of the field in the endzones
#'
#' @importFrom graphics lines plot points polygon segments text
#' @return Plots a football field
#' @export
#'
#' @examples
#' draw.football()

draw.football <- function(length = 120, width = 53+(1/3),
                          main = "Football", col1 = "palegreen3",
                          col2 = "white", col3 = "midnightblue") {

  # Create half variables for easy access to midfield shapes
  len.half <- length / 2
  wid.half <- width / 2

  # Create data frame of polygon definitions
  coords <- data.frame(
    Polygon = c('Field', 'Boundary', 'LEndzone', 'REndzone'),
    X1 = c(-3, 0, 0, length),
    X2 = c(length + 3, length, 10, length - 10),
    Y1 = c(-3, 0, 0, 0),
    Y2 = c(width + 3, rep(width, 3)),
    Color = c(rep(col1, 2), rep(col3, 2)),
    Border = c(col1, rep(col2, 3)),
    stringsAsFactors = FALSE
  )

  # Create data frame of segment definitions
  seg.coords <- data.frame(
    X0 = c(seq(15, 105, by = 5), rep(11:(120 - 11), 4), 0, length),
    Y0 = c(
      rep(0, 19), # Major cross-field segments
      rep(0, 99), # Bottom hash marks
      rep(width * (1 / 3), 99), # Second has marks
      rep(width * (2 / 3) - 1.5, 99), # Third hash marks
      rep(width - 1.5, 99), # Top has marks
      rep(wid.half + (37 / 6), 2)
    ),
    Y1 = c(
      rep(width, 19),
      rep(1.5, 99),
      rep(width * (1 / 3) + 1.5, 99),
      rep(width * (2 / 3), 99),
      rep(width, 99),
      rep(wid.half - (37 / 6), 2)
    ),
    Width = c(rep(2, 19), rep(1, 99 * 4), 7, 7),
    Color = c(rep(col2, 19 + 99 * 4), rep('goldenrod', 2)),
    stringsAsFactors = FALSE
  )

  # Define plot boundaries
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

  # Draw field and field boundaries
  sapply(1:nrow(coords), function(i) {
    polygon(
      x = c(coords$X1[i], coords$X1[i], coords$X2[i], coords$X2[i]),
      y = c(coords$Y1[i], coords$Y2[i], coords$Y2[i], coords$Y1[i]),
      lwd = 3,
      col = coords$Color[i],
      border = coords$Border[i]
    )
  })

  # Draw line segments
  sapply(1:nrow(seg.coords), function(i) {
    segments(
      x0 = seg.coords$X0[i],
      y0 = seg.coords$Y0[i],
      x1 = seg.coords$X0[i],
      y1 = seg.coords$Y1[i],
      col = seg.coords$Color[i],
      lwd = seg.coords$Width[i]
    )
  })

  # Add labels
  text.seq <- seq(20, length - 20, by = 10)
  text.labs <- c("1 0", "2 0", "3 0", "4 0", "5 0", "4 0", "3 0", "2 0", "1 0")
  text(text.seq, 9, text.labs, col = col2)
  text(text.seq, width - 9, text.labs, col = col2)
}
