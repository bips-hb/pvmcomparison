#' Load Data
#'
#' Returns the data from a number of repetitions.
#'
#' @inheritParams returnFilename
#'
#' @return data in a data frame. The column \code{repetition} represents the repetition
#' @export
loadData <- function(n_reports,
                     n_drugs,
                     n_events,
                     alpha_drugs,
                     beta_drugs,
                     alpha_events,
                     beta_events,
                     n_correlated_drugs,
                     rho_drugs,
                     n_correlated_events,
                     rho_events,
                     n_correlated_pairs,
                     rho,
                     repetitions = c(1),
                     type = "raw") {
  # allocates memory for the data
  data <- dplyr::tibble()
  
  
  for (repetition in repetitions) {
    filename <- pvmcomparison::returnFilename(
      n_reports = n_reports,
      n_drugs = n_drugs,
      n_events = n_events,
      alpha_drugs = alpha_drugs,
      beta_drugs = beta_drugs,
      alpha_events = alpha_events,
      beta_events = beta_events,
      n_correlated_drugs = n_correlated_drugs,
      rho_drugs = rho_drugs,
      n_correlated_events = n_correlated_events,
      rho_events = rho_events,
      n_correlated_pairs = n_correlated_pairs,
      rho = rho,
      repetitions = c(repetition),
      type = type
    )
    data_rep <- readr::read_rds(filename)
    data_rep$repetition <- repetition
    data <- dplyr::bind_rows(data, data_rep)
  }
  return(data)
}