#' Simulation Data File Name for a DAG-based SR Data Set
#'
#' Returns the simulation data file name given a parameter setting
#'
#' @param n_reports Number of reports
#' @param n_drugs Number of drugs
#' @param n_events Number of adverse drug events
#' @param alpha_drugs Alpha parameter for the drug marginal probabilities
#' @param beta_drugs Beta parameter for the drug marginal probabilities
#' @param alpha_events Alpha parameter for the event marginal probabilities
#' @param beta_events Beta parameter for the event marginal probabilities
#' @param method The method used for generating the random DAG for the drugs
#' @param exp_degree Average degree between the drugs in the random DAG
#' @param theta_drugs The increase in odds-ratio when there is a connection between two drugs
#' @param n_correlated_pairs Number of drug-event pairs that will exhibit a nonzero correlation
#' @param theta Increase in odds-ratio when there is an edge going from a drug to an event
#' @param repetitions An array of repetitions one want to get
#' @param type Type of data. Takes the values \code{"raw"} for the raw data,
#'             \code{"results"} for the table with the output of the methods in
#'             the function \code{\link{applyBatteryOfMethods}} and \code{"auc"}
#'             for the AUC filenames.
#'
#' @return A vector of filenames
#' @export
returnFilename <- function(n_reports,
                           n_drugs,
                           n_events,
                           alpha_drugs,
                           beta_drugs,
                           alpha_events,
                           beta_events,
                           n_innocent_bystanders,
                           theta_drugs,
                           n_correlated_pairs,
                           theta,
                           repetitions,
                           type = "raw") {
  filenames = c()
  
  for (repetition in repetitions) {
    if (length(theta_drugs) == 1 & length(theta) == 1) {
      filename <-
        sprintf(
          "_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%d_%.3f_%d_%.3f_%d.RDS",
          n_reports,
          n_drugs,
          n_events,
          alpha_drugs,
          beta_drugs,
          alpha_events,
          beta_events,
          n_innocent_bystanders,
          theta_drugs,
          n_correlated_pairs,
          theta,
          repetition
        )
    } else if (length(theta_drugs) != 1 & length(theta) == 1) {
      filename <-
        sprintf(
          "_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%d_%.3f_%.3f_%d_%.3f_%d.RDS",
          n_reports,
          n_drugs,
          n_events,
          alpha_drugs,
          beta_drugs,
          alpha_events,
          beta_events,
          n_innocent_bystanders,
          theta_drugs[1],
          theta_drugs[2],
          n_correlated_pairs,
          theta,
          repetition
        )
    } else if (length(theta_drugs) == 1 & length(theta) != 1) {
      filename <-
        sprintf(
          "_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%d_%.3f_%d_%.3f_%.3f_%d.RDS",
          n_reports,
          n_drugs,
          n_events,
          alpha_drugs,
          beta_drugs,
          alpha_events,
          beta_events,
          n_innocent_bystanders,
          theta_drugs,
          n_correlated_pairs,
          theta[1],
          theta[2],
          repetition
        )
    } else {
      filename <-
        sprintf(
          "_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%d_%.3f_%.3f_%d_%.3f_%.3f_%d.RDS",
          n_reports,
          n_drugs,
          n_events,
          alpha_drugs,
          beta_drugs,
          alpha_events,
          beta_events,
          n_innocent_bystanders,
          theta_drugs[1],
          theta_drugs[2],
          n_correlated_pairs,
          theta[1],
          theta[2],
          repetition
        )
    }
    
    
    if (type == "raw") {
      filename <- paste("data/dag", filename, sep = "")
    } else if (type == "results") {
      filename <- paste("results/dag", filename, sep = "")
    } else {
      filename <- paste("results/dag_auc", filename, sep = "")
    }
    filenames <- c(filenames, filename)
  }
  return(filenames)
}