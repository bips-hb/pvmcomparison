############################################
# run.R
#
# Script that executes all steps for 
# generating the simulation data and 
# the results
############################################

sim_param = as_tibble(
  expand.grid(
    n_reports = c(50000),
    n_drugs = c(500),
    n_events = c(500),
    alpha_drugs = c(1.0),
    beta_drugs = c(20.0),
    alpha_events = c(1.0),
    beta_events = c(20.0),
    n_innocent_bystanders = c(0, 125, 250),
    bystander_prob = c(0.5, 0.75, 0.9),
    n_correlated_pairs = c(250),
    theta = c(1.5, 3.0, 5.0),
    repetition = 1:50
  )
) %>% dplyr::filter(!(n_innocent_bystanders == 0 &
                        bystander_prob > 0.5))

source("exec/generate-simulation-data.R")

source("exec/generate-results.R")

source("exec/createFigures.R")

