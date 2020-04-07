# Determine the average no. of drugs and adverse events on a report 
# for the different parameter settings

library(pvmcomparison)
library(ggplot2)
library(latex2exp) 
library(gridExtra)

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


### Create plots with the distributions
range <- 1:500
prob <- sapply(1:500, function(s) prob_n_drugs_per_report(s, 500, 0, .9))

ggplot(data = data.frame(range = range, prob = prob)) + 
  geom_bar(aes(x = range, y = prob), stat = "identity", width=1, fill = "blue", alpha = .4) + 
  scale_x_continuous(limits = c(8,60), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.09), expand = c(0, 0)) +
  xlab("number of drugs") + 
  ylab("probability") + 
  theme_bw()


ggplot(data = data.frame(range = range, prob = prob)) + 
  geom_line(aes(x = range, y = prob)) + 
  scale_x_continuous(limits = c(8,60), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.09), expand = c(0, 0)) +
  xlab("number of drugs") + 
  ylab("probability") + 
  theme_bw()


return_data <- function(m0, m1, g) { 
  data.frame(
    range = 1:500, 
    p = sapply(1:500, function(s) prob_n_drugs_per_report(s, m0, m1, g))
  )
}


#' Function for generating a distribution plot for the AEs and the 
#' drugs when there are no innocent bystanders
create_plot1 <- function(title, xlabel) { 
  prob1 <- return_data(500, 0, .5)
  
  ggplot(NULL, aes(range, p)) + 
    geom_line(aes(color = "dEQ"), data = prob1, stat = "identity") +
    scale_x_continuous(limits = c(8,60), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,.09), expand = c(0, 0)) +
    xlab(xlabel) + 
    ylab("probability") + 
    ggtitle(title) +  
    theme_bw() + 
    theme(legend.position = "none") 
}

#' Function for generating a plot for the # drug distribution. 
#' All three gamma values are plotted.  
create_plot3 <- function(m0, m1) { 
  
  prob1 <- return_data(m0, m1, .5)
  prob2 <- return_data(m0, m1, .75)
  prob3 <- return_data(m0, m1, .9)  
  
  ggplot(NULL, aes(range, p)) + 
    geom_line(aes(color = "dEQ"), data = prob1, stat = "identity") +
    geom_line(aes(color = "LMD"), data = prob2, stat = "identity") + 
    geom_line(aes(color = "X"), data = prob3, stat = "identity") + 
    scale_x_continuous(limits = c(8,60), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,.09), expand = c(0, 0)) +
    xlab("number of drugs") + 
    ylab("probability") + 
    ggtitle(sprintf("%d innocent bystanders", m1)) +  
    scale_color_discrete(labels = unname(TeX(c("$\\gamma = .5", "$\\gamma = .75", "$\\gamma = .9")))) + 
    theme_bw() + 
    theme(legend.text.align = 0, 
          legend.title = element_blank()) 
}


p1 <- create_plot1("number of AEs", "number of AEs") 
p2 <- create_plot1("no innocent bystanders", "number of drugs") 
p3 <- create_plot3(375, 125) 
p4 <- create_plot3(250, 250) 

p_all <- grid.arrange(p1, p2, p3, p4, nrow = 2)


ggsave(
  "figures/distributions_n_drugs_AEs.pdf", 
  plot = p_all,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 24,
  height = 16,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)

ggsave(
  "figures/distribution_n_AEs.pdf", 
  plot = p1,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 14,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)

ggsave(
  "figures/distribution_drugs_no_IBs.pdf", 
  plot = p2,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 14,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)


ggsave(
  "figures/distribution_drugs_125_IBs.pdf", 
  plot = p3,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 14,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)

ggsave(
  "figures/distribution_drugs_250_IBs.pdf", 
  plot = p4,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 14,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)


### Possibility to check whether the simulated data matched the analytical outcome: 
# 
# # get all the AUC results 
# filenames <- list.files("data/", pattern = "dag_50000_500_500_1.000_20.000_1.000_20.000_125_0.900_250_1.500_+")
# 
# # determine the mean (output generated for each file)
# v = vector()
# for (f in filenames) { 
#   f = paste("data/", f, sep = "", collapse = NULL)
#   print(f)
#   x = readRDS(f)
#   s = x$sr
#   v = c(v, mean(rowSums(s[,1:500])))
#   print(mean(v))
# }






