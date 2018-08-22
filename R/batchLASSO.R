#' Batch LASSO
#'
#' This function can be used to rank the response-exposure pairs from most
#' to least "interesting". Simply using the resulting regression coefficients
#' would be unwise, since the size of the coefficients cannot be compared
#' from model to model. Instead, we use the tuning parameter, \eqn{\lambda}.
#' \cr\cr
#' Each response variable in the given \code{response} matrix is regressed
#' on all exposures in the matrix \code{exposure}. We determine for each
#' exposure what the highest value of \eqn{\lambda} for which that variable
#' is included for the first time in the regression model (i.e., its
#' regression coefficient is non-zero). These \eqn{\lambda}-values can be
#' compared across models.
#'
#' @param response A binary matrix where each column is a response variable
#' @param exposure A binary matrix where each column is an exposure
#' @param alpha The elastic net mixing parameter (Default: 1.0 - LASSO)
#' @param verbose Verbosity (Default: \code{TRUE})
#'
#' @return A data frame with three columns
#'         \item{response}{The response label (in case they are not given,
#'         they are simply numbered 1,2,3,...etc.)}
#'         \item{exposure}{The exposure label (in case they are not given,
#'         they are simply numbered 1,2,3,...etc.)}
#'         \item{highest_lambda}{The highest lambda for which the
#'         response-exposure pair where first added to the active set. The
#'         higher this value, the more "interesting" the pair}
#'
#' @examples \dontrun{
#' n <- 100 # no. of observations
#' r <- 5 # no. of response variables
#' e <- 10 # no. of exposures
#'
#' # random response and exposure matrices
#' response <- matrix(rbinom(r*n, 1, 0.5), n, r)
#' exposure <- matrix(rbinom(e*n, 1, 0.5), n, e)
#'
#' batchLASSO(response, exposure)
#' }
#'
#' @export
batchLASSO <- function(response, exposure, alpha = 1.0, verbose = TRUE) {
  
  response <- as.matrix(response)
  exposure <- as.matrix(exposure)
  
  if (nrow(response) != nrow(exposure)) {
    stop("The number of rows for the response and the exposure should be the same")
  }
  
  n_responses <- ncol(response)
  n_exposures <- ncol(exposure)
  
  ### set up the results data frame
  response_labels <- colnames(response)
  if (is.null(response_labels)) {
    response_labels <- 1:n_responses
  }
  
  exposure_labels <- colnames(exposure)
  if (is.null(exposure_labels)) {
    exposure_labels <- 1:n_exposures
  }
  
  result <- expand.grid(response_labels, exposure_labels, -1)
  colnames(result) <- c("response", "exposure", "highest_lambda")
  
  if (verbose) { # create a progress bar
    pb <- txtProgressBar(min = 0, max = n_responses, initial = 0, style = 3)
  }
  
  # go over all individual response variables and apply the LASSO
  for (r in 1:n_responses) {
    y <- unlist( response[, r] )
    
    if (verbose) {
      setTxtProgressBar(pb, r)
    }
    
    # check whether there enough observations
    if (sum(y) > 1 & sum(y) < (length(y)-1)) {
      
      # fit the LASSO
      fit <- glmnet::glmnet(exposure, y, alpha = alpha, family = "binomial")
      
      # determine the active sets for all lambdas
      active_sets <- fit$beta != 0
      
      # get for which lambda the exposure variable appears for the first time
      # in the active set
      first_appearance <- apply(active_sets == TRUE, 1, which.max) - 1
      # in case there is no first appearance, i.e., there was no lambda
      # found for that particular variable, we set it to NA, so that we
      # can later change it to 0
      first_appearance[first_appearance == 0] <- NA
      
      # the highest lambdas for which the variables appear for the first time
      highest_lambda <- fit$lambda[first_appearance]
      # in case no lambda was found for that exposure, highest_lambda is set to 0
      highest_lambda[is.na(highest_lambda)] <- 0
      
      result$highest_lambda[((r-1)*n_exposures+1):(r*n_exposures)] <- highest_lambda
      
    } else {
      warning(
        sprintf(
          "response column %d contains only 1 or 0. Regression was not applied to this column",
          r
        )
        
      )
    }
  }
  
  result$highest_lambda[result$highest_lambda == -1] <- 0
  
  return(result)
}
