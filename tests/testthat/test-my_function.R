# Load required package
library(testthat)
library(mdatools)
library(asdnprivatefunctions)

test_that("ASDN_evaluate_all_components returns a data.frame", {
  # Generate dummy data
  set.seed(123)
  X <- matrix(rnorm(100 * 10), nrow = 100)
  y <- rnorm(100)

  # Create a valid pls model with 5 components and 5-fold CV
  pls_model <- pls(X, y, ncomp = 5, cv = 5, scale = TRUE)

  # Run your function
  result <- ASDN_evaluate_all_components(pls_model)

  # Test the output is a data.frame
  expect_s3_class(result, "data.frame")
})

# Load required package
library(testthat)
library(mdatools)
library(asdnprivatefunctions)

test_that("ASDN_evaluate_all_components returns a data.frame", {
  # Generate dummy data
  set.seed(123)
  X <- matrix(rnorm(100 * 10), nrow = 100)
  y <- rnorm(100)

  # Create a valid pls model with 5 components and 5-fold CV
  pls_model <- pls(X, y, ncomp = 5, cv = 5, scale = TRUE)

  # Run your function
  result <- ASDN_evaluate_all_components(pls_model)

  # Test the output is a data.frame
  expect_s3_class(result, "data.frame")
})

test_that("ASDN_final_model_assessment returns expected structure or NULL", {
  set.seed(123)
  X <- matrix(rnorm(100 * 10), nrow = 100)
  y <- rnorm(100)
  pls_model <- pls(X, y, ncomp = 5, cv = 5, scale = TRUE)

  result <- ASDN_final_model_assessment(pls_model)

  # Either a list with Conclusion, or NULL if model failed
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_true("Conclusion" %in% names(result))
  } else {
    message("⚠️ Function returned NULL — likely due to poor model quality.")
    succeed()
  }
})
