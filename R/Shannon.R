#' shannon - Shannon index and evenness calculating function
#'
#' @description Calculation of Shannon index and evenness
#' @usage shannon(df, first.col = 2, table = "CE", plot = T,  log = "e", ylab = "Shannon index value", col = "black", ...)
#' @param df A data frame containing uploaded user dataset (list of taxa in first column, followed by columns of abundances with sample names in a header).
#' @param first.col Numeric (integer), the first column of samples (to skip non-relevant columns). By default, this parameter is set to ‘2’.
#' @param table Table output type. In case the table is equal to ‘C’ the table will be created only for Shannon calculations (Shannon index, the lower limit of Shannon index and the highest limit of Shannon index), for table equal to ‘E’ the output will be the table for evenness of Shannon index. In the case of table equal to ‘CE’ a sheet of two table type elements will be created for both Shannon calculations and evenness.
#' @param plot Should a plot for results of calculations be plotted? By default, the plot is rendered.
#' @param log Logarithm base type. The possibilities are ‘e’, for a base equal to Euler's number and ‘2’, for a base equal to two.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Shannon index value’.
#' @param col A vector that contains five components (named after the five colours). By default, the color is set as ‘gray’.
#' @param ... xxx
#'
#' @return A data frame consisting of a column of calculated species richness values (‘F1’, ‘F2’, ‘Other’, ‘Total’) with samples in the rows.
#'
#' @examples
#' # Popsat nastavení následujících argumentů¨
#'
#' shannon(test_data, log = "2", table = "C")
#'
#' @export shannon
#'


shannon <- function(df, first.col = 2, table = "CE", plot = T,
                    log = "e", ylab = "Shannon index value", col = "black", ...){
  data.shan <- df[, first.col : ncol(df)]
  mat1 <- matrix(nrow = 3)
  Htable <- data.frame(mat1)
  mat2 <- matrix(nrow = 2)
  Etable <- data.frame(mat2)

  if (log == "e"){
    Base <- sum(1 / factorial(0:100))
  }

  if (log == "2"){
    Base <- 2
  }

  for(x in 1:ncol(data.shan)){
    x <- data.shan[, x]
    n <- sum(x[x > 0])
    S <- length(x[x > 0])
    Hshan <- - sum(x[x > 0] / n * log(x[x > 0] / n, Base))
    Hmin <- -((n-S+1) / n * log((n - S + 1) / n, Base) + (S - 1) / n * log(1 / n, Base))
    Hmax <- log(S, Base)
    Even1 <- Hshan / (Hmax)
    Even2 <- (Hshan - Hmin) / (Hmax-Hmin)
    vec1 <- c(Hshan, Hmin, Hmax)
    Htable <- cbind(Htable, new_col = vec1)

    vec2 <- c(Even1, Even2)
    Etable <- cbind(Etable, new_col = vec2)
  }

  Htable <- Htable[, -1]
  colnames(Htable) <- colnames(data.shan)
  rownames(Htable) <- c("H´","Hmin","Hmax")
  Htable <- round(Htable, digits = 2)
  Etable <- Etable[,-1]
  colnames(Etable) <- colnames(data.shan)
  rownames(Etable) <- c("Even1","Even2")
  Etable <- round(Etable, digits = 2)

  if (plot == T){
    par(mfrow = c(1, 1), mar = c(5, 4, 1, 0.5), las = 2)
    minH <- min(Htable[2,])
    maxH <- max(Htable[3,])
    posgr = barplot(as.matrix(Htable[1,]), plot = F)
    plot(NULL,ylim = c(minH, maxH * 1.1),xlim = c(1,ncol(data.shan)), xlab = "", xaxt = "n",
         ylab = "",...)
    title(ylab = ylab, line = 2.5)
    points(c(1:ncol(Htable)), Htable[1,], pch = 16, col = col)

    table1 <- as.matrix(Htable[2,])
    table2 <- as.matrix(Htable[3,])
    axis(1, at = 1:ncol(data.shan), lab = colnames(data.shan))

    for (i in 1:ncol(table1)){
      arrows(i, table1[,i], i, table2[,i], angle = 90, code = 3, length = 0.08, col = col)
    }
  }

  if (table == "CE"){
    HEtable <- list( "Shannon calculations" = as.data.frame(t(Htable)), "Shannon evenness" = as.data.frame(t(Etable)))
    return(HEtable)
  }

  if(table == "C"){
    Htable <- as.data.frame(t(Htable))
    return(Htable)
  }

  if(table == "E"){
    Etable <- as.data.frame(t(Etable))
    return(Etable)
  }
}
