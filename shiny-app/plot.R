# contains the plotting functions ---

plotBoxplot <-
  function(data,
           ylim = NULL,
           title = "",
           measure = "PRCAUC",
           font_size = 12) {
    
    if (is.null(ylim)) {
      ylim <- c(
        max(0.0, min(data[[measure]], na.rm = TRUE) - 0.01), 
        min(1.0, max(data[[measure]], na.rm = TRUE) + 0.01)
      )
    }
    
    data$method <-
      factor(data$method, levels = unique(data$method[order(data$score)]))
    data$method_label <-
      factor(data$method_label, levels = unique(data$method_label[order(data$score)]))
    ml <- as.vector(unlist(lapply(unique(data$method_label), TeX)))
    
    if (measure == "AUC") {
    p <- ggplot(data) +
      geom_boxplot(aes(x = method, y = AUC), fill = 'grey90') +
      ylab("AUC") 
    } else if (measure == 'PRCAUC') { 
      p <- ggplot(data) +
        geom_boxplot(aes(x = method, y = PRCAUC), fill = 'grey90') +
        ylab("Area under the PRC")
    } else { 
      p <- ggplot(data) +
        geom_boxplot(aes(x = method, y = percentageNA), fill = 'grey90') + 
        ylab("Fraction Not Defined")
    }
    
    p + scale_x_discrete(breaks = unique(data$method), labels = ml) +
      scale_y_continuous(limits = ylim, expand = c(0, 0)) +
      coord_flip() +
      xlab("") +
      ggtitle(title) +
      theme_bw() +
      theme(legend.position = "none", axis.text=element_text(size=font_size),
            axis.title=element_text(size=font_size,face="bold"))
  }


plotSettings <-
  function(data, 
           ylim = NULL,
           title = "") {
    
    
    labels = c(
      '1' = 'no bystanders',
      '2' = parse(text = TeX("$\\gamma = .5$")),
      '3' = parse(text = TeX("$\\gamma = .75$")),
      '4' = parse(text = TeX("$\\gamma = .9$")),
      '5' = parse(text = TeX("$\\gamma = .5$")),
      '6' = parse(text = TeX("$\\gamma = .75$")),
      '7' = parse(text = TeX("$\\gamma = .9$"))
    )
    
    
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
