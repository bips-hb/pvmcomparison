#' Plot for the Different Parameter Settings
#'
#' Returns a boxplot with the AUCs for one particular method.
#'
#' @return A plot
#' @export
plotSettingsDAG <-
  function(method,
           theta = 5.0,
           ylim = NULL,
           method_name = method,
           measure = 'PRCAUC',
           return_table = FALSE,
           pattern = "aucdag+") {
    if (!(measure %in% c("AUC", "PRCAUC"))) {
      measure <- "percentageNA"
    }
    
    
    if (theta == 5.0) {
      title <-
        TeX(sprintf("%s - strong assocation ($OR \\approx 5$)", method_name))
    } else if (theta == 3.0) {
      title <-
        TeX(sprintf("%s - medium assocation ($OR \\approx 3$)", method_name))
    } else if (theta == 1.5) {
      title <-
        TeX(sprintf("%s - weak assocation ($OR \\approx 1.5$)", method_name))
    }
    
    # get all the AUC results
    filenames <- list.files("results/", pattern = pattern)
    
    data <- tibble()
    
    for (filename in filenames) {
      data_auc <- readr::read_rds(paste("results/", filename, sep = ""))
      data <- dplyr::bind_rows(data, data_auc)
    }
    
    method_ <- method
    theta_ <- theta
    
    data <- data %>% filter(method == method_)
    
    data$theta <- as.factor(data$theta)
    data$bystander_prob <- as.factor(data$bystander_prob)
    data$n_innocent_bystanders <-
      as.factor(data$n_innocent_bystanders)
    
    data <- data %>% dplyr::filter(theta == theta_)
    
    
    # set the setting for each
    data <- data %>% rowwise() %>% dplyr::mutate(
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
    
    if (is.null(ylim)) {
      ylim <- c(max(0.0, min(data[[measure]], na.rm = TRUE) - 0.01),
                min(1.0, max(data[[measure]], na.rm = TRUE) + 0.01))
    }
    
    data$id <- as.factor(data$id)
    
    if (return_table) {
      return(data)
    }
    #
    # labels <-
    #   c(
    #     "no bystanders",
    #     "$\\gamma$",
    #     "125 (medium)",
    #     "125 (strong)",
    #     "250 (weak)",
    #     "250 (medium)",
    #     "250 (strong)"
    #   )
    
    labels = c(
      '1' = 'no bystanders',
      '2' = parse(text = TeX("$\\gamma = .5$")),
      '3' = parse(text = TeX("$\\gamma = .75$")),
      '4' = parse(text = TeX("$\\gamma = .9$")),
      '5' = parse(text = TeX("$\\gamma = .5$")),
      '6' = parse(text = TeX("$\\gamma = .75$")),
      '7' = parse(text = TeX("$\\gamma = .9$"))
    )
    
    if (measure == 'AUC') {
      p <- ggplot(data = data) +
        geom_boxplot(aes(x = id, y = AUC), fill = 'grey90') +
        #scale_x_discrete(breaks = unique(data$method), labels = ml) +
        scale_y_continuous(expand = c(0, 0), limits = ylim) +
        #coord_flip() +
        xlab("") +
        ylab("AUC") +
        ggtitle(title) +
        theme_bw() +
        theme(legend.position = "none") +
        geom_vline(xintercept = 1.5, linetype = "dotted") +
        geom_vline(xintercept = 4.5, linetype = "dotted") +
        theme(
          panel.grid.major.x = element_line(size = .1, color = "grey"),
          panel.grid.major.y = element_blank()
        ) + theme(axis.text.x = element_text(angle = 0)) +
        scale_x_discrete(labels = labels)
    } else if (measure == 'PRCAUC') {
      p <- ggplot(data = data) +
        geom_boxplot(aes(x = id, y = PRCAUC), fill = 'grey90') +
        #scale_x_discrete(breaks = unique(data$method), labels = ml) +
        scale_y_continuous(expand = c(0, 0), limits = ylim) +
        #coord_flip() +
        xlab("") +
        ylab("Area under the PRC") +
        ggtitle(title) +
        theme_bw() +
        theme(legend.position = "none") +
        geom_vline(xintercept = 1.5, linetype = "dotted") +
        geom_vline(xintercept = 4.5, linetype = "dotted") +
        theme(
          panel.grid.major.x = element_line(size = .1, color = "grey"),
          panel.grid.major.y = element_blank()
        ) + theme(axis.text.x = element_text(angle = 0)) +
        scale_x_discrete(labels = labels)
    } else {
      p <- ggplot(data = data) +
        geom_boxplot(aes(x = id, y = percentageNA), fill = 'grey90') +
        #scale_x_discrete(breaks = unique(data$method), labels = ml) +
        scale_y_continuous(expand = c(0, 0), limits = ylim) +
        #coord_flip() +
        xlab("") +
        ylab("Fraction Not Defined") +
        ggtitle(title) +
        theme_bw() +
        theme(legend.position = "none") +
        geom_vline(xintercept = 1.5, linetype = "dotted") +
        geom_vline(xintercept = 4.5, linetype = "dotted") +
        theme(
          panel.grid.major.x = element_line(size = .1, color = "grey"),
          panel.grid.major.y = element_blank()
        ) + theme(axis.text.x = element_text(angle = 0)) +
        scale_x_discrete(
          labels = c(
            "classic",
            "125 (weak)",
            "125 (medium)",
            "125 (strong)",
            "250 (weak)",
            "250 (medium)",
            "250 (strong)"
          )
        )
      
    }
    
    g <- ggplotGrob(p)
    g <-
      gtable_add_grob(
        g,
        grobTree(
          textGrob(
            "125 innocent bystanders",
            x = unit(0.4, "npc"),
            y = unit(0.03, "npc"),
            gp = gpar(fontsize = 9, col = "grey20")
          ),
          textGrob(
            "250 innocent bystanders",
            x = unit(0.8, "npc"),
            y = unit(0.03, "npc"),
            gp = gpar(fontsize = 9, col = "grey20")
          )
        ),
        t = 1,
        l = 1,
        b = 10,
        r = 7
      )
    
    p <- grid.draw(g)
    
    # ## Add the second title and plot
    # g2 <- gtable_add_grob(g, textGrob("Right", x=1, hjust=1, gp=title_style),
    #                       t=2, l=4, b=2, r=4, name="right-title")
    
    return(p)
  }