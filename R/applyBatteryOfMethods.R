#' Apply Battery of Methods
#' 
#' Applies a battery of methods to a simultaneous 
#' reporting data set 
#' 
#' @param reports The raw SR data set, a binary matrix 
#' @param n_drugs,n_events The number of drugs and the number of events in the data set
#' @param tables A data frame with the 2x2 tables. 
#'               Must contain the columns \code{drug_id}, \code{event_id}, 
#'               \code{a}, \code{b}, \code{c} and \code{d}.
#'               In case it is not given, it is derived from the reports. 
#' @param verbose Verbosity (Default: \code{TRUE})
#' 
#' @return The \code{tables} data frame with a column for each method
#' @export 
applyBatteryOfMethods <- function(reports, n_drugs, n_events, 
                                  tables = NULL, 
                                  verbose = TRUE) {
  
  if (is.null(tables)) {
    tables <- pvm::convertRawReports2Tables(reports, n_drugs, n_events)
  }
  
  # sort the tables by event_id 
  # useful cause the LASSO method returns it results in the same way
  tables <- dplyr::arrange(tables, event_id) 
  
  ### Apply all the methods ------------------------------------------------

  if (verbose) {
    cat("Applying ROR...\n") 
  }
  
  tables$ROR       <- pvm::ROR(tables$a, tables$b, tables$c, tables$d)
  tables$ROR025    <- pvm::ROR(tables$a, tables$b, tables$c, tables$d, alpha = 0.025)
  tables$ROR05     <- pvm::ROR(tables$a, tables$b, tables$c, tables$d, alpha = 0.05)

  if (verbose) {
    cat("Applying Yule's Q...\n") 
  }
  
  tables$Q         <- pvm::YulesQ(tables$a, tables$b, tables$c, tables$d)
  tables$Q025      <- pvm::YulesQ(tables$a, tables$b, tables$c, tables$d, alpha = 0.025)
  tables$Q05       <- pvm::YulesQ(tables$a, tables$b, tables$c, tables$d, alpha = 0.05)

  if (verbose) {
    cat("Applying PRR...\n") 
  }
  
  tables$PRR       <- pvm::PRR(tables$a, tables$b, tables$c, tables$d)
  tables$PRR025    <- pvm::PRR(tables$a, tables$b, tables$c, tables$d, alpha = 0.025)
  tables$PRR05     <- pvm::PRR(tables$a, tables$b, tables$c, tables$d, alpha = 0.05)

  if (verbose) {
    cat("Applying RRR...\n") 
  }
  
  tables$RRR       <- pvm::RRR(tables$a, tables$b, tables$c, tables$d)

  if (verbose) {
    cat("Applying Log-Likelihood Ratio test...\n") 
  }
  
  tables$lbinomial <- pvm::logLikelihoodRatioBinomial(tables$a, tables$b, tables$c, tables$d)

  if (verbose) {
    cat("Applying the original BCPNN...\n") 
  }
  
  tables$ICOrig    <- pvm::BCPNN(tables$a, tables$b, tables$c, tables$d)
  tables$ICOrig025 <- pvm::BCPNN(tables$a, tables$b, tables$c, tables$d, alpha = 0.025)
  tables$ICOrig05  <- pvm::BCPNN(tables$a, tables$b, tables$c, tables$d, alpha = 0.05)

  if (verbose) {
    cat("Applying the new BCPNN...\n") 
  }
  
  tables$ICAlt     <- pvm::BCPNN(tables$a, tables$b, tables$c, tables$d, version = 'alternative')
  tables$ICAlt025  <- pvm::BCPNN(tables$a, tables$b, tables$c, tables$d, version = 'alternative', alpha = 0.025, mc_estimate = TRUE, mc_runs = 1000)
  tables$ICAlt05   <- pvm::BCPNN(tables$a, tables$b, tables$c, tables$d, version = 'alternative', alpha = 0.05, mc_estimate = TRUE, mc_runs = 1000)

  if (verbose) {
    cat("Applying the GPS...\n")
  }

  prior <- pvm::fitPriorParametersGPS(tables$a, tables$b, tables$c, tables$d) 
  tables$GPS       <- pvm::GPS(tables$a, tables$b, tables$c, tables$d, prior = prior)
  tables$GPS025    <- pvm::GPS(tables$a, tables$b, tables$c, tables$d, prior = prior, alpha = 0.025)
  tables$GPS05     <- pvm::GPS(tables$a, tables$b, tables$c, tables$d, prior = prior, alpha = 0.05)

  if (verbose) {
    cat("Applying the Poisson test...\n") 
  }
  
  tables$ppoisson  <- pvm::PoissonTest(tables$a, tables$b, tables$c, tables$d)

  if (verbose) {
    cat("Applying the Chi-Squared test...\n") 
  }
  
  tables$chi2      <- pvm::chi2Test(tables$a, tables$b, tables$c, tables$d, yates = FALSE)
  tables$chi2Yates <- pvm::chi2Test(tables$a, tables$b, tables$c, tables$d, yates = TRUE)

  if (verbose) {
    cat("Applying two versions of Fisher's Exact test...\n") 
  }
  
  tables$RFET      <- pvm::fisherExactTest(tables$a, tables$b, tables$c, tables$d, midpvalue = FALSE)
  tables$midRFET   <- pvm::fisherExactTest(tables$a, tables$b, tables$c, tables$d, midpvalue = TRUE)

  ### Apply the LASSO -----------------------------------
  
  if (verbose) {
    cat("Applying the LASSO...\n") 
  }

  res <- batchLASSO(response = reports[,(n_drugs+1):(n_drugs+n_events)],
                    exposure = reports[,1:n_drugs])
  tables$highest_lambda <- res$highest_lambda
  
  
  return(tables)
}