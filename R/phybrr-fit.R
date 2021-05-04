#' Fit a `phybrr`
#'
#' `phybrr()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `phybrr` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- phybrr(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- phybrr(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- phybrr(rec, mtcars)
#'
#' @export
phybrr <- function(x, ...) {
  UseMethod("phybrr")
}

#' @export
#' @rdname phybrr
phybrr.default <- function(x, ...) {
  stop("`phybrr()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname phybrr
phybrr.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  phybrr_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname phybrr
phybrr.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  phybrr_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname phybrr
phybrr.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  phybrr_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname phybrr
phybrr.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  phybrr_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

phybrr_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <- phybrr_impl(predictors, outcome)

  new_phybrr(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

phybrr_impl <- function(predictors, outcome) {
  list(coefs = 1)
}
