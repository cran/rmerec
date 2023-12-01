
test_doi_result <- function(result){
  # Normalization test
  expect_equal(
    round(as.vector(result$nij[, "C1"]), digits = 2),
    c(0.01, 0.50, 0.05, 0.02, 1.00)
  )
  expect_equal(
    round(as.vector(result$nij[, "C2"]), digits = 2),
    c(1.00, 0.88, 0.98, 0.86, 0.95)
  )
  expect_equal(
    round(as.vector(result$nij[, "C3"]), digits = 2),
    c(1.00, 0.04, 0.57, 0.02, 0.43)
  )
  expect_equal(
    round(as.vector(result$nij[, "C4"]), digits = 2),
    c(0.90, 0.99, 0.94, 1.00, 0.98)
  )

  # Alternative´s overall performance calculation test
  expect_equal(
    round(as.vector(result$Si[]), digits = 2),
    c(0.77, 0.71, 0.65, 1.09, 0.21)
  )

  # Alternative´s performance by removing each criterion
  expect_equal(
    round(as.vector(result$Sij[,]), digits = 2),
    c(0.03, 0.62, 0.15, 0.71, 0.21, 0.77, 0.69, 0.64, 1.08, 0.20, 0.77, 0.19, 0.57, 0.68, 0.02, 0.75, 0.71, 0.64, 1.09, 0.20)
  )

  # Sum of absolute deviations
  expect_equal(
    round(as.vector(result$Ej[]), digits = 2),
    c(1.71, 0.04, 1.19, 0.03)
  )

  # Final weights
  expect_equal(
    round(as.vector(result$wj[]), digits = 4),
    c(0.5752, 0.0141, 0.4016, 0.0091)
  )
}

test_that("Test data from DOI doi.org/10.3390/sym13040525", {

  alternatives <- c("A1", "A2", "A3", "A4", "A5")
  optimizations <- c("max", "max", "min", "min")
  data <- matrix(c(
    c(450, 10, 100, 220, 5),
    c(8000, 9100, 8200, 9300, 8400),
    c(54, 2, 31, 1, 23),
    c(145, 160, 153, 162, 158)
  ), nrow=5, ncol=4)

  # test errors
  expect_error({
    aux <- optimizations
    aux[1] <- 'xx'
    result <- merec_weights(data, alternatives, aux)
  })


  result <- merec_weights(data, alternatives, optimizations)
  test_doi_result(result)
})
