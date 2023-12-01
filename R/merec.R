#' Method based on the Removal Effects of Criteria - MEREC
#' Implementation of the MEthod based on the Removal Effects of Criteria - MEREC
#' More information about the method at https://doi.org/10.3390/sym13040525
#' More information about the implementation at https://github.com/lucassp/rmerec
#' Given a decision matrix, the function return the Merec weight´s vector.
#' @importFrom utils count.fields
#' @param data A numeric data matrix in the format of a DECISION MATRIX, columns are the criteria, rows are the alternatives
#' @param alternatives A character vector with the identification of alternatives
#' @param optimizations A character vector with definition of minimization or maximization for each criterion, expected 'min' or 'max' only
#' @returns A numeric vector with MEREC Weights (wj) and all matrix/vectors used to calculate it
#' @export
#' @examples
#' alternatives <- c("A1", "A2", "A3", "A4", "A5")
#' optimizations <- c("max", "max", "min", "min")
#'
#' data <- matrix(c(
#'   c(450, 10, 100, 220, 5),          # criterion 1 values
#'   c(8000, 9100, 8200, 9300, 8400),  # criterion 2 values
#'   c(54, 2, 31, 1, 23),              # criterion 3 values
#'   c(145, 160, 153, 162, 158)        # criterion 4 values
#' ), nrow=5, ncol=4)
#'
#' result <- merec_weights(data, alternatives, optimizations)

merec_weights <- function(data, alternatives, optimizations) {
  tryCatch({

    imax <- nrow(data)
    jmax <- ncol(data)

    # optimizations validation
    for (j in 1:jmax) {
      if (optimizations[j] == 'max') {
        optimizations[j] <- 1
      } else if (optimizations[j] == 'min') {
        optimizations[j] <- -1
      } else{
        stop("Only 'min' or 'max' are valid for criteria optimization")
      }
    }

    optimizations <- as.numeric(optimizations)

    # Calculate linear normalization
    nij <- matrix(0, imax, jmax)
    colnames(nij) <- paste("C", 1:jmax, sep="")  #adding criteria collumn names
    rownames(nij) <- alternatives                #adding alternatives names
    for (j in 1:jmax) {
      if (optimizations[j] == 1) {
        min <- min(data[, j])
        for (i in 1:imax) {
          nij[i, j] <- min / as.numeric(data[i, j])
        }
      } else if (optimizations[j] == -1) {
        max <- max(data[, j])
        for (i in 1:imax) {
          nij[i, j] <- as.numeric(data[i, j] / max)
        }
      }
    }

    # Calculate alternative´s general performance - Si: Overall Performance Matrix

    Si <- log(1 + (1/jmax * rowSums(abs(log(nij)))))

    # Calculate alternative´s performance by removing each criteria - Sij Matrix

    Sij <- matrix(0, nrow = nrow(nij), ncol = ncol(nij))
    for (j in 1:jmax) {
      ex_nmatrix <- nij[, -j, drop = FALSE]
      Sij[, j] <- log(1 + (1/jmax * rowSums(abs(log(ex_nmatrix)))))
    }

    # Sum of total deviations - Ej vector

    Ej <- matrix(0, jmax, 1)
    Ej <- colSums(abs(Sij - Si))

    # Determine the MEREC weight of each criteria - wj vector

    wj <- matrix(0, jmax, 1)
    wj <- round(Ej / sum(Ej),4)

    return(list("wj"= wj, "nij" = nij, "Si" = Si, "Sij" = Sij, "Ej" = Ej))
  },
  error = function(err) {
    stop(paste("Error: ", err))
  })
}
