#' menhinick - Menhinick's index
#'
#' @description Calculates Menhinick’s index and displays the results of the calculation in graphical and tabular form.
#' @usage menhinick(x, plot = T, col = "gray", ylab = "Menhinick´s index", xlab = "Samples", ...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Menhinick´s index value.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col The colour used in the plot. By default, it is set as ‘gray’.
#' @param ... Arguments to be passed to methods, such graphical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated Menhinick’s index (‘D’) with samples in the rows.
#'
#' @examples
#' # Calculation of the Menhinick’s index for the 'test_data' and plotted results. The colour used is 'darkcyan'. The data frame of calculation results is stored in an object called 'menhinick_tab'.
#'
#' menhinick_tab <- menhinick(test_data.1, col = "darkcyan")
#'
#' @export menhinick

menhinick <- function(x, plot = T, col = "gray",
                      ylab = "Menhinick´s index value", xlab = "Samples", ...){
  x <- x[,(attributes(x)$"First column"):ncol(x)]
  S <- apply(x, 2, function(x1) sum(x1 > 0))
  N <- apply(x, 2, sum, na.rm = TRUE)
  tab <- rbind(S, N)
  tabS <- matrix(S, nrow = 1)
  tabN <- matrix(N, nrow = 1)
  DMn_m <- matrix()

  for (i in 1:ncol(tab)){
    DMn <- (tabS[,i]) / sqrt(tabN[,i])
    DMn_m <- rbind(DMn_m, DMn)
  }

  DMn_m <- as.data.frame(DMn_m[2:nrow(DMn_m),1])
  colnames(DMn_m) <- "D"
  DMn_m <- round(DMn_m, digits = 2)
  DMn <- DMn_m[,1,drop = F]
  rownames(DMn) <- as.vector(colnames(tab))
  maxMn <- max(DMn)
  plot_Dmn <- as.table(t(DMn_m))
  if (plot == T){
    par(mfrow = c(1,1), mar = c(5, 4, 0.5, 0.5), las = 2)
    posgr <- barplot(plot_Dmn, plot = F)
    barplot(plot_Dmn, ylim = c(0, maxMn * 1.25), names.arg = rownames(DMn), col = col, xaxs = "r", ...)
    text(posgr, plot_Dmn, lab = plot_Dmn, cex = 0.65, adj = c(-0.2,0.3), srt = 90)
    title(ylab = ylab, line = 2.5)
    title(xlab = xlab, line = 4)
  }

  return(DMn)
}
