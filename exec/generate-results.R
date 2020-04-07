library(pvmcomparison)
library(batchtools)
library(pvm)
library(tidyverse)

# set up the registry ------------------------------------------

reg_name <- "resultsPVM"
reg_dir <- sprintf("%s/registries/%s", getwd(), reg_name)
unlink(reg_dir, recursive = TRUE)
makeRegistry(file.dir = reg_dir)
#loadRegistry(reg_dir, writeable = TRUE) 

results_generator <- function(n_reports, n_drugs, n_events, 
                              alpha_drugs, beta_drugs, 
                              alpha_events, beta_events, 
                              n_innocent_bystanders,
                              bystander_prob, 
                              n_correlated_pairs, theta,
                              repetition) {
  # get the filename
  filename_in <-
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
  
  filename_results <- gsub("data", "results", filename_in)
  filename_auc <- gsub("data/dag", "results/aucdag", filename_in)
  
  if (!file.exists(filename_results)) {
    
    # read in raw data  
    data <- readr::read_rds(filename_in)
    
    # apply the methods 
    tables <- pvmcomparison::applyBatteryOfMethods(data$sr, n_drugs, n_events, tables = data$tables)
    
    readr::write_rds(tables, filename_results)
  } 
  else {
    tables <- readr::read_rds(filename_results)
  }
  
  # get the AUC and other measures
  truth <- tables$associated 
  
  methods <- dplyr::tibble(
    method = c("a", colnames(tables)[11:36]),
    decreasing = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F, T),
    percentageNA = NA,
    AUC = NA,
    PRCAUC = NA
  )
  
  # get the AUC and other measures
  truth <- tables$associated 

  for (row in 1:nrow(methods)) {
    method <- as.character(methods[row, "method"])
    decr <- as.logical(methods[row, "decreasing"])
    
    methods[row, "percentageNA"] <- sum(is.na(tables[method])) / nrow(tables)  
    
    scores <- tables[[method]]
    if (!decr) { 
      scores <- 1 - tables[[method]]   
    }
    
    temp <- precrec::evalmod(scores = scores, labels = truth)
    methods[row, "AUC"] <- precrec::auc(temp)$aucs[1]
    methods[row, "PRCAUC"] <- precrec::auc(temp)$aucs[2]
  }
  
  methods <- dplyr::mutate(dplyr::select(methods, -(decreasing)) ,
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
                           theta = theta,
                           repetition = repetition
  )
  
  readr::write_rds(methods, filename_auc)
  
  return(filename_results) 
}

batchtools::batchMap(results_generator, args = sim_param)

ids <- batchtools::findJobs() 

# Submit -----------------------------------------------------------
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- batchtools::findNotDone(ids = ids)
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              ncpus = 1, memory = 6000, walltime = 10*24*3600,
                              max.concurrent.jobs = 50))
} else {
  ids <- batchtools::findNotStarted(ids = ids)  
  submitJobs(ids)
}
waitForJobs()

#getStatus()
# 
#getErrorMessages()
