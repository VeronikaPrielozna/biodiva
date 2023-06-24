#' simpson - Simpson’s index and evenness
#'
#' @description Calculates Simpson’s index and evenness and displays the results of the calculation in graphical and tabular form.
#' @usage simpson(x, plot = T, ylab = "Simpson's index value", xlab = "Samples", col = "gray",...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Simpson´s index value’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col The colour used in the plot. By default, it is set as ‘gray’.
#' @param ... Arguments to be passed to methods, such grapfical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated Simpson’s calculations ans evenness (‘D´’, ‘Dc’, ‘Dr’, ‘Even’) with samples in the rows.
#'
#' @examples
#' # Calculation of the Simpson’s calculations for the 'test_data' and plotted results. The colour used is 'darkcyan'. Data frame of the results is stored in an object called 'simpson_tab'.
#'
#' simpson_tab <- simpson(test_data, col = "darkcyan")
#'
#' @export simpson

simpson <- function(x, plot = T, ylab = "Simpson's index value", xlab = "Samples", col = "gray",...){
  data.simps <- x[,(attributes(x)$"First column"):ncol(x)]
  mat <- matrix(nrow = 4, ncol = ncol(data.simps))
  tab <- data.frame(mat)
  D <- apply(data.simps, 2, function(x) sum((x / sum(x)) ^ 2))
  Dc <- 1 - D
  Dr <- 1 / D
  S <- apply(data.simps, 2, function(x1) sum(x1 > 0))
  Even <- Dc / S
  tab[1,] <- D
  tab[2,] <- Dc
  tab[3,] <- Dr
  tab[4,] <- Even

  colnames(tab) <- colnames(data.simps)
  rownames(tab) <- c("D", "Dc", "Dr", "Even")
  tab <- round(tab, digits = 2)

  if (plot == T){
    par(mfrow = c(1, 1), mar = c(5, 4, 0.5, 0.5), las = 2)
    maxD <- max(tab[2, ])
    posgr <- barplot(Dc, plot = F)
    barplot(Dc, ylim = c(0, maxD * 1.2), col = col, ...)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
    Dc1 <- round(Dc, digits = 2)
    text(posgr, Dc1, lab = Dc1, cex = 0.65, adj = c(-0.2,0.3), srt = 90)
  }
  tab <- as.data.frame(t(tab))
  return(tab)
}
