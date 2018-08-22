# get all the AUC results 
filenames <- list.files("results/", pattern = "aucdag+")

data <- tibble() 

print(filenames)

for (filename in filenames) { 
  data_auc <- readr::read_rds(paste("results/", filename, sep=""))
  data <- dplyr::bind_rows(data, data_auc)
}

data$method <- as.character(data$method)
data$theta <- as.factor(data$theta)
data$bystander_prob <- as.factor(data$bystander_prob)
data$n_innocent_bystanders <- as.integer(data$n_innocent_bystanders)
data$n_reports <- as.integer(data$n_reports) 
data$n_drugs <- as.integer(data$n_drugs)
data$n_events <- as.integer(data$n_events) 
data$n_correlated_pairs <- as.integer(data$n_correlated_pairs)

data <- data %>% dplyr::filter(method != "lowest_lambda")

readr::write_rds(data, "results.rds")