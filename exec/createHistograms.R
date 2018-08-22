


library(SRSim)
library(pvmcomparison)
library(ggplot2)

createHistogram <- function(n_reports = 50000,
                            n_drugs = 500,
                            n_events = 500,
                            n_innocent_bystanders = 250,
                            bystander_prob = 0.9,
                            n_correlated_pairs = 250,
                            theta = 3.0,
                            theta_std = 1.0) {
  res <-
    SRSim::simulateSR(
      n_reports = n_reports,
      n_drugs = n_drugs,
      n_events = n_events,
      n_innocent_bystanders = n_innocent_bystanders,
      bystander_prob = bystander_prob,
      theta = c(theta, theta_std),
      n_correlated_pairs = n_correlated_pairs,
      valid_reports = T
    )
  
  tables <- create2x2TablesDAG(res) %>% mutate(bystander = FALSE)
  tables$est_or <- pvm::ROR(tables$a, tables$b, tables$c, tables$d)
  
  if (n_innocent_bystanders > 0) {
    for (i in 1:nrow(tables)) {
      table <- tables[i,]
      if (table$drug_id <= n_innocent_bystanders &
          table$associated) {
        bystander_id <- table$drug_id + (n_drugs / 2)
        index <- (bystander_id - 1) * n_events + table$event_id
        tables[index,]$bystander <- TRUE
      }
    }
  }
  
  tables <- tables %>% mutate(class = ifelse(
    associated,
    'associated',
    ifelse(bystander, 'bystander', 'not associated')
  ))
  
  
  # Density plots
  ggplot(tables) + geom_density(aes(
    x = est_or,
    group = class,
    colour = class,
    fill = class
  ), alpha = 0.3) +
    coord_cartesian(xlim = c(0, theta + 5), ylim = c(0, 2))
}


sim_param = as_tibble(
  expand.grid(
    n_reports = c(50000),
    n_drugs = c(500),
    n_events = c(500),
    n_innocent_bystanders = c(0, 125, 250),
    bystander_prob = c(0.5, 0.75, 0.9),
    n_correlated_pairs = c(250),
    theta = c(1.5, 3, 5)
  )
) %>% dplyr::filter(!(n_innocent_bystanders == 0 &
                        bystander_prob > 0.5))

for (i in 1:nrow(sim_param)) {
  p <- sim_param[i,]
  print(p)
  
  filename <- sprintf("figures/hist_%d_%g_%g.pdf",
                      p$n_innocent_bystanders,
                      p$bystander_prob,
                      p$theta)
  if (!file.exists(filename)) {
    ggsave(
      sprintf(
        "figures/hist_%d_%g_%g.pdf",
        p$n_innocent_bystanders,
        p$bystander_prob,
        p$theta
      ),
      plot = createHistogram(
        theta = p$theta,
        bystander_prob = p$bystander_prob,
        n_innocent_bystanders = p$n_innocent_bystanders
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







# # Density plots
# ggplot(tables %>% filter(class != 'not associated')) + geom_histogram(aes(
#   x = est_or,
#   group = class,
#   colour = class,
#   fill = class
# ), alpha = 0.3) +
#   coord_cartesian(xlim = c(0, theta + 5), ylim = c(0, 2))
#
# # Density plots
# ggplot(tables %>% filter(class != 'not associated'),
#        aes(x = est_or, colour = class)) + geom_density()
#
# # Density plots
# ggplot(tables %>% filter(class != 'not associated'),
#        aes(x = est_or, fill = class)) + geom_histogram()
