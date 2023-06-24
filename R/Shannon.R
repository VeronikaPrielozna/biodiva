#' Shannon index and evenness
#'
#' @description Calculates Shannon index and evenness and displays the results of the calculation in graphical and tabular form.
#' @usage shannon(x, table = "CE", plot = T,  log = "e", ylab = "Shannon index value", xlab = "Samples", col = "black", ...)
#'
#' @param x A data frame uploaded by the loadData function (important because of the first column setting) containing the uploaded user dataset (list of taxa in the first column, followed by abundance columns with sample names in a header).
#' @param table Table output type. In case the table is equal to ‘C’ the table will be created only for Shannon calculations (Shannon index, the lower limit of Shannon index and the highest limit of Shannon index), for table equal to ‘E’ the output will be the table for evenness of Shannon index. In the case of table equal to ‘CE’ a sheet of two table type elements will be created for both Shannon calculations and evenness.
#' @param plot Should a plot for results of calculations be plotted? By default, the plot is rendered.
#' @param log Logarithm base type. The possibilities are ‘e’, for a base equal to Euler's number and ‘2’, for a base equal to two.
#' @param ylab The text for the y axis label in plot. By default, the text is setted as ‘Shannon index value’.
#' @param xlab The text for the x axis label. By default, the text is setted as ‘Samples’.
#' @param col The colour used in the plot. By default, it is set as ‘gray’.
#' @param ... Arguments to be passed to methods, such graphical parameters. Many methods will acccept the following arguments.
#'
#' @return A data frame consisting of a column of calculated Shannon calculations (‘H´’, ‘Hmin’, ‘Hmax’) and evenness (‘Even1’, ‘Even2’) with samples in the rows.
#'
#' @examples
#' # Calculation of the Shannon calculations for the 'test_data' and plotted results. The base of the logaritm is set to 2'. Data frame of the results is stored in an object called 'shannon_tab'.
#'
#' shannon_tab <- shannon(test_data, log = "2", table = "C")
#'
#' @export shannon

shannon <- function(x, table = "CE", plot = T,
                    log = "e", ylab = "Shannon index value", xlab = "Samples" ,col = "black", ...){
  data.shan <- x[,(attributes(x)$"First column"):ncol(x)]
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
    title(xlab = xlab, line = 4)
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
