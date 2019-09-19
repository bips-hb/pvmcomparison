#' Boxplot of the AUCs
#'
#' Returns a boxplot with the AUCs for each method.
#'
#' @return A ROC plot
#' @export
plotAUCDAG <-
  function(theta = 1.5,
           bystander_prob = 0.9,
           n_innocent_bystanders = 250,
           ylim = NULL,
           title = "",
           measure = 'PRCAUC',
           font_size = 12, 
           pattern = "aucdag+") {
  
  if (!(measure %in% c("AUC", "PRCAUC"))) { 
    measure <- "percentageNA" 
  }
      
  # get all the AUC results 
  filenames <- list.files("results/", pattern = pattern)
  
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
  
  theta_ <- theta 
  bystander_prob_ <- bystander_prob
  n_innocent_bystanders_ <- n_innocent_bystanders
  
  data <- data %>% filter(
    method != 'lowest_lambda', # old LASSO version
    bystander_prob == bystander_prob_, 
    n_innocent_bystanders == n_innocent_bystanders_,
    theta == theta_)
  
  # filter out Q 
  data <- data %>% filter(!(method %in% c("Q", "Q025", "Q05")))
  
  if (is.null(ylim)) {
    ylim <- c(
      max(0.0, min(data[[measure]], na.rm = TRUE) - 0.01), 
      min(1.0, max(data[[measure]], na.rm = TRUE) + 0.01)
    )
  }
  
  if (measure == 'AUC') {
    data <- data %>%
      group_by(method) %>%
      mutate(mean_auc = mean(AUC, na.rm = TRUE)) %>%
      arrange(mean_auc) %>%
      ungroup()
  } else if (measure == 'PRCAUC') {
    data <- data %>%
      group_by(method) %>%
      mutate(mean_auc = mean(PRCAUC, na.rm = TRUE)) %>%
      arrange(mean_auc) %>%
      ungroup()
  } else {
    data <- data %>%
      group_by(method) %>%
      mutate(mean_na = mean(percentageNA, na.rm = TRUE)) %>%
      arrange(mean_na) %>%
      ungroup()
  }
  
  
  data$method_label <- data$method
  
  data$method_label <- replace(data$method_label, data$method_label == "a", "# reports")
  data$method_label <- replace(data$method_label, data$method_label == "chi2", "$\\chi^2$")
  data$method_label <- replace(data$method_label, data$method_label == "chi2Yates", "$\\chi^2_{Yates}$")
  data$method_label <- replace(data$method_label, data$method_label == "GPS", "EBGM")
  data$method_label <- replace(data$method_label, data$method_label == "GPS025", "$EB_{025}$")
  data$method_label <- replace(data$method_label, data$method_label == "GPS05", "$EB_{05}$")
  data$method_label <- replace(data$method_label, data$method_label == "ICAlt", "$IC^{alternative}$")
  data$method_label <- replace(data$method_label, data$method_label == "ICAlt025", "$IC^{alternative}_{025}$")
  data$method_label <- replace(data$method_label, data$method_label == "ICAlt05", "$IC^{alternative}_{05}$")
  data$method_label <- replace(data$method_label, data$method_label == "ICOrig", "$IC^{original}$")
  data$method_label <- replace(data$method_label, data$method_label == "ICOrig025", "$IC^{original}_{025}$")
  data$method_label <- replace(data$method_label, data$method_label == "ICOrig05", "$IC^{original}_{05}$")
  data$method_label <- replace(data$method_label, data$method_label == "lbinomial", "$\\Lambda_{binomial}$")
  data$method_label <- replace(data$method_label, data$method_label == "highest_lambda", "LASSO")
  data$method_label <- replace(data$method_label, data$method_label == "midRFET", "midRFET")
  data$method_label <- replace(data$method_label, data$method_label == "ppoisson", "$p_{Poisson}$")
  data$method_label <- replace(data$method_label, data$method_label == "PRR", "PRR")
  data$method_label <- replace(data$method_label, data$method_label == "PRR025", "$PRR_{025}$")
  data$method_label <- replace(data$method_label, data$method_label == "PRR05", "$PRR_{05}$")
  data$method_label <- replace(data$method_label, data$method_label == "Q", "$Q$")
  data$method_label <- replace(data$method_label, data$method_label == "Q025", "$Q_{025}$")
  data$method_label <- replace(data$method_label, data$method_label == "Q05", "$Q_{05}$")
  data$method_label <- replace(data$method_label, data$method_label == "RFET", "RFET")
  data$method_label <- replace(data$method_label, data$method_label == "ROR", "ROR")
  data$method_label <- replace(data$method_label, data$method_label == "ROR025", "$ROR_{025}$")
  data$method_label <- replace(data$method_label, data$method_label == "ROR05", "$ROR_{05}$")
  data$method_label <- replace(data$method_label, data$method_label == "RRR", "RRR")
  
  if (measure == 'AUC' | measure == 'PRCAUC') {
    data$method <-
      factor(data$method, levels = unique(data$method[order(data$mean_auc)]))
    data$method_label <-
      factor(data$method_label, levels = unique(data$method_label[order(data$mean_auc)]))
  } else {
    data$method <-
      factor(data$method, levels = unique(data$method[order(data$mean_na)]))
    data$method_label <-
      factor(data$method_label, levels = unique(data$method_label[order(data$mean_na)]))
  }
  ml <- as.vector(unlist(lapply(unique(data$method_label), TeX)))
  
  if (measure == "AUC") {
    p <- ggplot(data) +
      geom_boxplot(aes(x = method, y = AUC), fill = 'grey90') +
      scale_x_discrete(breaks = unique(data$method), labels = ml) +
      scale_y_continuous(limits = ylim, expand = c(0, 0)) +
      coord_flip() +
      xlab("") +
      ylab("AUC") +
      ggtitle(title) +
      theme_bw() +
      theme(legend.position = "none", axis.text=element_text(size=font_size),
            axis.title=element_text(size=font_size,face="bold"))
  } else if (measure == 'PRCAUC') { 
    p <- ggplot(data) +
      geom_boxplot(aes(x = method, y = PRCAUC), fill = 'grey90') +
      scale_x_discrete(breaks = unique(data$method), labels = ml) +
      scale_y_continuous(limits = ylim, expand = c(0, 0)) +
      coord_flip() +
      xlab("") +
      ylab("Area under the PRC") +
      ggtitle(title) +
      theme_bw() +
      theme(legend.position = "none", axis.text=element_text(size=font_size),
            axis.title=element_text(size=font_size,face="bold"))
  } else {
    p <- ggplot(data) +
      geom_boxplot(aes(x = method, y = percentageNA), fill = 'grey90') +
      scale_x_discrete(breaks = unique(data$method), labels = ml) +
      scale_y_continuous(limits = ylim, expand = c(0, 0)) +
      coord_flip() +
      theme_grey(base_size = font_size) +
      theme(text = element_text(size=font_size)) + 
      xlab("") +
      ylab("Fraction Not Defined") +
      ggtitle(title) +
      theme_bw()  +
      theme(legend.position = "none", axis.text=element_text(size=font_size),
                             axis.title=element_text(size=font_size,face="bold"))
  }
  return(p)
}