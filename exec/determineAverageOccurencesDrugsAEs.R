# Determine the average no. of drugs and adverse events on a report 
# for the different parameter settings

library(pvmcomparison)

#' The probability of observing a number of drugs on a report
#' 
#' @param s number of drugs
#' @param m0 number of 'ordinary' drugs
#' @param m1 number of innocent bystanders
#' @param g gamma 
#' 
#' @return probability
prob_n_drugs_per_report <- function(s, m0, m1, g) { 
  
  if (m0 <= s) { 
    if (m1 <= s) { 
      k_min = s - m1
      k_max = m0
    } else {
      k_min = 0
      k_max = m0
    }
  } 
  
  if (m0 > s) { 
    if (m1 <= s) { 
      k_min = s - m1
      k_max = s
    } else { 
      k_min = 0
      k_max = s
    }
  }
  
  total = 0
  for (k in k_min:k_max) { 
    total <- total + choose(m0, k)*choose(m1, s-k)*(1/21)^k*(20/21)^(m0 - k)*(1/21*(g + 20/21))^(s - k)*(1 - 1/21*(g + 20/21))^(m1 - s + k) 
  }
  total
}

#' Probability whether the first selected drug is an innocent bystander
#'
#' @param m0 number of 'ordinary' drugs
#' @param m1 number of innocent bystanders
#' @param g gamma 
#' 
#' @return probability
prob_first_drug_innocent_bystander <- function(m0, m1, g) { 
  q1 <- m1/21*(g + 20/21)
  q0 <- m0/21
  q1 / (q0 + q1)
}

#' Expected number of drugs per report 
#' 
#' @param m0 number of 'ordinary' drugs
#' @param m1 number of innocent bystanders
#' @param g gamma 
#' 
#' @return Expectation
exp_n_drugs_per_report = function(m0, m1, g) { 
  
  # either the first drug selected is an 'ordinary' drug, or an 
  # innocent bystander
  prob_bystander <- prob_first_drug_innocent_bystander(m0, m1, g)
  
  # when first drug is an innocent bystander: 
  x1 = sapply(0:(m0 + m1 - 1), function(i) prob_n_drugs_per_report(i, m0, m1 - 1, g))
  
  # when first drug is not an innocent bystander: 
  x0 = sapply(0:(m0 + m1 - 1), function(i) prob_n_drugs_per_report(i, m0 - 1, m1, g))
  
  x = x1*prob_bystander + x0*(1 - prob_bystander)
  sum(x*(0:(m0+m1-1)))
}

# All the parameter settings used in the paper
sim_param = as_tibble(
  expand.grid(
    n_innocent_bystanders = c(0, 125, 250),
    bystander_prob = c(0.5, 0.75, 0.9)
  )
) %>% dplyr::filter(!(n_innocent_bystanders == 0 &
                        bystander_prob > 0.5)) 

res <- sim_param %>% rowwise() %>% 
  mutate(
    exp_n_drugs = exp_n_drugs_per_report(500 - n_innocent_bystanders, n_innocent_bystanders, bystander_prob), 
    exp_n_AEs = 499 / 21
  )

### Possibility to check whether the simulated data matched the analytical outcome: 

# get all the AUC results 
filenames <- list.files("data/", pattern = "dag_50000_500_500_1.000_20.000_1.000_20.000_125_0.900_250_1.500_+")

# determine the mean (output generated for each file)
v = vector()
for (f in filenames) { 
  f = paste("data/", f, sep = "", collapse = NULL)
  print(f)
  x = readRDS(f)
  s = x$sr
  v = c(v, mean(rowSums(s[,1:500])))
  print(mean(v))
}






