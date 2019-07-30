
library(pvmcomparison)

pattern <- "threshold4_"

# get all the AUC results 
filenames <- list.files("results/", pattern = pattern)

data <- tibble() 

print(filenames)

for (filename in filenames) { 
  data_auc <- readr::read_rds(paste("results/", filename, sep=""))
  data <- dplyr::bind_rows(data, data_auc)
}

ylim <- c(
  max(0.0, min(data$PRCAUC, na.rm = TRUE) - 0.01), 
  min(1.0, max(data$PRCAUC, na.rm = TRUE) + 0.01)
)

for (theta in unique(sim_param$theta)) {
  
  theta_ <- theta 
  d <- dplyr::filter(data, theta == theta_) 
  ylim <- c(
    max(0.0, min(d$PRCAUC, na.rm = TRUE) - 0.01), 
    min(1.0, max(d$PRCAUC, na.rm = TRUE) + 0.01)
  )
  
  for (n_innocent_bystanders in unique(sim_param$n_innocent_bystanders)) {
    if (n_innocent_bystanders == 0) {
      bystander_probs <- c(0.5) 
    } else {
      bystander_probs <- c(0.5, 0.75, 0.9)
    }
    for (bystander_prob in bystander_probs) {
      
      if (n_innocent_bystanders == 0) {
        title <- TeX(sprintf("OR$\\approx %g$, no innocent bystanders", theta)) 
      } else { 
        title <- TeX(sprintf("OR$\\approx %g$, %d innocent bystanders, $\\gamma = %g$", 
                             theta, n_innocent_bystanders, bystander_prob))  
      }
      
      ggsave(
        sprintf(
          "figures/srsim_%.3f_%.3f_%d.pdf",
          theta,
          bystander_prob,
          n_innocent_bystanders
        ),
        plot = pvmcomparison::plotAUCDAG(
          theta = theta,
          bystander_prob = bystander_prob,
          n_innocent_bystanders = n_innocent_bystanders,
          ylim = ylim,
          measure = "PRCAUC",
          title = title,
          font_size = 13,
          pattern = pattern
        ),
        device = NULL,
        path = NULL,
        scale = 1,
        width = 174,
        height = 200,
        units = "mm",
        dpi = 1000,
        limitsize = TRUE
      )
    }
  }
}



for (theta in unique(sim_param$theta)) {
  for (n_innocent_bystanders in unique(sim_param$n_innocent_bystanders)) {
    if (n_innocent_bystanders == 0) {
      bystander_probs <- c(0.5) 
    } else {
      bystander_probs <- c(0.5, 0.75, 0.9)
    }
    for (bystander_prob in bystander_probs) {
      
      ggsave(
        sprintf(
          "figures/percna_%.3f_%.3f_%d.pdf",
          theta,
          bystander_prob,
          n_innocent_bystanders
        ),
        plot = pvmcomparison::plotAUCDAG(
          theta = theta,
          bystander_prob = bystander_prob,
          n_innocent_bystanders = n_innocent_bystanders,
          ylim = NULL,
          measure = "percna",
          pattern = pattern
        ),
        device = NULL,
        path = NULL,
        scale = 1,
        width = 20,
        height = 16,
        units = "cm",
        dpi = 1000,
        limitsize = TRUE
      )
    }
  }
}


# plot for settings




### SETTINGS PLOTS #################################


labels <- data.frame(
  method = c("a",
             "chi2", 
             "chi2Yates", 
             "GPS", 
             "GPS025", 
             "GPS05", 
             "ICAlt", 
             "ICAlt025", 
             "ICAlt05", 
             "ICOrig", 
             "ICOrig025", 
             "ICOrig05", 
             "lbinomial",
             "highest_lambda", 
             "midRFET", 
             "ppoisson", 
             "PRR", 
             "PRR025",
             "PRR05", 
             "Q", 
             "Q025", 
             "Q05", 
             "RFET", 
             "ROR", 
             "ROR025", 
             "ROR05", 
             "RRR"),
  method_label = c("# reports",
                   "$\\chi^2$",
                   "$\\chi^2_{Yates}$",
                   "EBGM",
                   "$EB_{025}$",
                   "$EB_{05}$",
                   "$IC^{alternative}$",
                   "$IC^{alternative}_{025}$",
                   "$IC^{alternative}_{05}$",
                   "$IC^{original}$",
                   "$IC^{original}_{025}$",
                   "$IC^{original}_{05}$",
                   "$\\Lambda_{binomial}$",
                   "LASSO",
                   "midRFET",
                   "$p_{Poisson}$",
                   "PRR",
                   "$PRR_{025}$",
                   "$PRR_{05}$",
                   "$Q$",
                   "$Q_{025}$",
                   "$Q_{05}$",
                   "RFET",
                   "ROR",
                   "$ROR_{025}$",
                   "$ROR_{05}$",
                   "RRR")
)

for (i in 1:27) { 
  meth = as.character(labels[i,]$method)  
  meth_label = as.character(labels[i,]$method_label)
  print(meth_label)
  
  for (theta in c(1.5, 3.0, 5.0)) {
    ggsave(
      sprintf("figures/dag_%s_%.3f.pdf", meth, theta),
      plot = pvmcomparison::plotSettingsDAG(method = meth, 
                                            theta = theta,
                                            method_name = meth_label,
                                            measure = "PRCAUC", 
                                            pattern = pattern
      ),
      device = NULL,
      path = NULL,
      scale = 1,
      width = 20,
      height = 8,
      units = "cm",
      dpi = 1000,
      limitsize = TRUE
    )
    
    # ggsave(
    #   sprintf("figures/percna_%s_%.3f.pdf", meth, theta),
    #   plot = pvmcomparison::plotSettingsDAG(method = meth, 
    #                                         theta = theta,
    #                                         method_name = meth_label,
    #                                         measure = "percna"
    #   ),
    #   device = NULL,
    #   path = NULL,
    #   scale = 1,
    #   width = 20,
    #   height = 8,
    #   units = "cm",
    #   dpi = 1000,
    #   limitsize = TRUE
    # )
  }
}

ylim <- c(.51, .98)

meth <- "ppoisson"
theta <- 5.0
meth_label <- "$p_{Poisson}$"

ggsave(
  sprintf("figures/figure3_%s_%.3f.pdf", meth, theta),
  plot = pvmcomparison::plotSettingsDAG(method = meth, 
                                        theta = theta,
                                        method_name = meth_label,
                                        measure = "PRCAUC",
                                        ylim = ylim,
                                        pattern = pattern
  ),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 20,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)


meth <- "ICAlt05"
theta <- 5.0
meth_label <- "$IC^{alternative}_{05}$"

ggsave(
  sprintf("figures/figure4_%s_%.3f.pdf", meth, theta),
  plot = pvmcomparison::plotSettingsDAG(method = meth, 
                                        theta = theta,
                                        method_name = meth_label,
                                        measure = "PRCAUC",
                                        ylim = ylim,
                                        pattern = pattern
  ),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 20,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)


meth <- "highest_lambda"
theta <- 5.0
meth_label <- "LASSO"

ggsave(
  sprintf("figures/figure4_%s_%.3f.pdf", meth, theta),
  plot = pvmcomparison::plotSettingsDAG(method = meth, 
                                        theta = theta,
                                        method_name = meth_label,
                                        measure = "PRCAUC",
                                        ylim = ylim,
                                        pattern = pattern
  ),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 20,
  height = 8,
  units = "cm",
  dpi = 1000,
  limitsize = TRUE
)





########### PLOTS FOR THE POSTER ##############

for (theta in unique(sim_param$theta)) {
  
  theta_ <- theta 
  d <- dplyr::filter(data, theta == theta_) 
  # ylim <- c(
  #   max(0.0, min(d$PRCAUC, na.rm = TRUE) - 0.01), 
  #   min(1.0, max(d$PRCAUC, na.rm = TRUE) + 0.01)
  # )
  
  for (n_innocent_bystanders in unique(sim_param$n_innocent_bystanders)) {
    if (n_innocent_bystanders == 0) {
      bystander_probs <- c(0.5) 
    } else {
      bystander_probs <- c(0.5, 0.75, 0.9)
    }
    for (bystander_prob in bystander_probs) {
      
      if (n_innocent_bystanders == 0) {
        title <- TeX(sprintf("OR$\\approx %g$, no innocent bystanders", theta)) 
      } else { 
        title <- TeX(sprintf("OR$\\approx %g$, %d innocent bystanders, $\\gamma = %g$", 
                             theta, n_innocent_bystanders, bystander_prob))  
      }
      
      ggsave(
        sprintf(
          "figures/poster_%.3f_%.3f_%d.eps",
          theta,
          bystander_prob,
          n_innocent_bystanders
        ),
        plot = pvmcomparison::plotAUCDAG(
          theta = theta,
          bystander_prob = bystander_prob,
          n_innocent_bystanders = n_innocent_bystanders,
          ylim = NULL,
          measure = "PRCAUC",
          title = title,
          font_size = 13,
          pattern = pattern
        ),
        device = "eps",
        path = NULL,
        scale = 1,
        width = 270,
        height = 300,
        units = "mm",
        dpi = 1000,
        limitsize = TRUE
      )
    }
  }
}


