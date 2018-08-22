# Load the results ---
results <- readr::read_rds("data/results.rds") 

# sort the method 
results <- results %>% dplyr::arrange(method) 

results$method_label <- results$method

results$method_label <- replace(results$method_label, results$method_label == "a", "# reports")
results$method_label <- replace(results$method_label, results$method_label == "chi2", "$\\chi^2$")
results$method_label <- replace(results$method_label, results$method_label == "chi2Yates", "$\\chi^2_{Yates}$")
results$method_label <- replace(results$method_label, results$method_label == "GPS", "EBGM")
results$method_label <- replace(results$method_label, results$method_label == "GPS025", "$EB_{025}$")
results$method_label <- replace(results$method_label, results$method_label == "GPS05", "$EB_{05}$")
results$method_label <- replace(results$method_label, results$method_label == "ICAlt", "$IC^{alternative}$")
results$method_label <- replace(results$method_label, results$method_label == "ICAlt025", "$IC^{alternative}_{025}$")
results$method_label <- replace(results$method_label, results$method_label == "ICAlt05", "$IC^{alternative}_{05}$")
results$method_label <- replace(results$method_label, results$method_label == "ICOrig", "$IC^{original}$")
results$method_label <- replace(results$method_label, results$method_label == "ICOrig025", "$IC^{original}_{025}$")
results$method_label <- replace(results$method_label, results$method_label == "ICOrig05", "$IC^{original}_{05}$")
results$method_label <- replace(results$method_label, results$method_label == "lbinomial", "$\\Lambda_{binomial}$")
results$method_label <- replace(results$method_label, results$method_label == "highest_lambda", "LASSO")
results$method_label <- replace(results$method_label, results$method_label == "midRFET", "midRFET")
results$method_label <- replace(results$method_label, results$method_label == "ppoisson", "$p_{Poisson}$")
results$method_label <- replace(results$method_label, results$method_label == "PRR", "PRR")
results$method_label <- replace(results$method_label, results$method_label == "PRR025", "$PRR_{025}$")
results$method_label <- replace(results$method_label, results$method_label == "PRR05", "$PRR_{05}$")
results$method_label <- replace(results$method_label, results$method_label == "Q", "$Q$")
results$method_label <- replace(results$method_label, results$method_label == "Q025", "$Q_{025}$")
results$method_label <- replace(results$method_label, results$method_label == "Q05", "$Q_{05}$")
results$method_label <- replace(results$method_label, results$method_label == "RFET", "RFET")
results$method_label <- replace(results$method_label, results$method_label == "ROR", "ROR")
results$method_label <- replace(results$method_label, results$method_label == "ROR025", "$ROR_{025}$")
results$method_label <- replace(results$method_label, results$method_label == "ROR05", "$ROR_{05}$")
results$method_label <- replace(results$method_label, results$method_label == "RRR", "RRR")


# set the setting for each
results <- results %>% rowwise() %>% dplyr::mutate(
  setting = ifelse(
    n_innocent_bystanders == 0,
    'classic',
    ifelse(
      n_innocent_bystanders == 125,
      '125 bystanders',
      '250 bystanders'
    )
  ),
  id = ifelse(
    setting == 'classic',
    1,
    ifelse(
      setting == '125 bystanders' & bystander_prob == 0.5,
      2,
      ifelse(
        setting == '125 bystanders' & bystander_prob == 0.75,
        3,
        ifelse(
          setting == '125 bystanders' & bystander_prob == 0.9,
          4,
          ifelse(
            setting == '250 bystanders' & bystander_prob == 0.5,
            5,
            ifelse(setting == '250 bystanders' &
                     bystander_prob == 0.75, 6,
                   7)
          )
        )
      )
    )
  )
) %>% ungroup()

results$id <- as.factor(results$id)