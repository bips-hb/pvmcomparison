library(pvmcomparison)
library(batchtools)
library(dplyr)
library(readr)
library(SRSim)
library(pcalg)
library(igraph)

set.seed(1)

# set up the registry ------------------------------------------

reg_name <- "simulateSRS"
reg_dir <- sprintf("%s/registries/%s", getwd(), reg_name)
unlink(reg_dir, recursive = TRUE)
makeRegistry(file.dir = reg_dir)
#loadRegistry(reg_dir, writeable = TRUE)

simulator_wrapper <- function(n_reports,
                              n_drugs,
                              n_events,
                              alpha_drugs,
                              beta_drugs,
                              alpha_events,
                              beta_events,
                              n_innocent_bystanders,
                              bystander_prob,
                              n_correlated_pairs,
                              theta,
                              repetition) {
  # get the filename
  filename <-
    pvmcomparison::returnFilename(
      n_reports,
      n_drugs,
      n_events,
      alpha_drugs,
      beta_drugs,
      alpha_events,
      beta_events,
      n_innocent_bystanders,
      bystander_prob,
      n_correlated_pairs,
      theta,
      repetition
    )
  
  # simulate the data set
  res <- SRSim::simulateSRS(
    n_reports = n_reports,
    n_drugs = n_drugs,
    n_events = n_events,
    alpha_drugs = alpha_drugs,
    beta_drugs = beta_drugs,
    alpha_events = alpha_events,
    beta_events = beta_events,
    n_innocent_bystanders = n_innocent_bystanders,
    bystander_prob = bystander_prob,
    n_correlated_pairs = n_correlated_pairs,
    theta = c(theta, 1),
    seed = NULL,
    valid_reports = TRUE,
    verbose = TRUE
  )
  
  # get the 2x2 tables as well
  tables <- SRSim::convert2Tables(res)
  
  readr::write_rds(
    list(
      sr = res$sr,
      prob_drugs = res$prob_drugs,
      prob_events = res$prob_events,
      adjecency_matrix = res$adjacency_matrix,
      nodes = res$nodes,
      tables = tables
    ),
    filename,
    compress = "gz"
  )
  
  return(filename)
}

batchtools::batchMap(simulator_wrapper, args = sim_param)

ids <- batchtools::findJobs(repetition < 10000)

# Submit -----------------------------------------------------------
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotDone(ids = ids)
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(
    ids = ids,
    # walltime in seconds, 10 days max, memory in MB
    resources = list(
      name = reg_name,
      chunks.as.arrayjobs = TRUE,
      ncpus = 3,
      memory = 64000,
      walltime = 10 * 24 * 3600,
      max.concurrent.jobs = 200
    )
  )
} else {
  ids <- findNotDone(ids = ids)
  submitJobs(ids)
}
waitForJobs()

#getStatus()

#getErrorMessages()
