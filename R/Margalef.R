#' Margalef’s index
#'
#' @description Calculates Margalef’s index and displays the results of the calculation in graphical and tabular form.
#' @usage margalef(x, plot = T, ylab = "Margalef's index value", xlab = "Samples", col = "gray", ...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param plot Should a barplot for results of calculations be plotted? By default, the plot is rendered.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Margalef's index value’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col The colour used in the plot. By default, it is set as ‘gray’.
#' @param ... Arguments to be passed to methods, such graphical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated Margalef’s index (‘D’) with samples in the rows.
#'
#' @examples
#' # Calculation of the Margalef’s index for the 'test_data' and plotted results. The colour used is 'darkcyan'. The data frame of calculation results is stored in an object called 'margalef_tab'.
#'
#' margalef_tab <- margalef(test_data, col = "darkcyan")
#'
#' @export margalef

margalef <- function(x, plot = T, ylab = "Margalef's index value", xlab = "Samples", col = "gray", ...){
  x <- x[,(attributes(x)$"First column"):ncol(x)]
  S <- apply(x, 2, function(x1) sum(x1 > 0))
  N <- apply(x, 2, sum, na.rm = TRUE)
  tab <- rbind(S, N)
  tabS <- matrix(S, nrow = 1)
  tabN <- matrix(N, nrow = 1)
  DMg_m <- matrix()

  for (i in 1:ncol(tab)){
    DMg <- (tabS[,i] - 1) / log(tabN[,i])
    DMg_m <- rbind(DMg_m, DMg)
  }

  DMg_m <- as.data.frame(DMg_m[2:nrow(DMg_m),1])
  colnames(DMg_m) <- "D"
  DMg_m <- round(DMg_m, digits = 2)
  DMg <- DMg_m[,1,drop = F]
  rownames(DMg) <- as.vector(colnames(tab))
  maxMg <- max(DMg)
  print(DMg)
  DMg_m_posgr <- as.table(t(DMg_m))
    if (plot == T){
    par(mfrow = c(1,1), mar = c(5, 4, 0.5, 0.5), las = 2)
    posgr <- barplot(DMg_m_posgr, plot = F)
    barplot(DMg_m_posgr, ylim = c(0, maxMg * 1.25), names.arg = rownames(DMg), col = col, xaxs = "r", ...)
    text(posgr, DMg_m_posgr, lab = DMg_m_posgr, cex = 0.65, adj = c(-0.2,0.3), srt = 90)
    title(ylab = ylab, line = 3)
    title(xlab = xlab, line = 4)
  }
   return(DMg)
}

