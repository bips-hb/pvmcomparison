#' Plot the Correlation Matrix
#'
#' Creates a heatmap of the correlation matrix.
#'
#' The code is based on
#' http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
#'
#' @param corrmat The correlation matrix.
#'
#' @return A heatmap of the correlation matrix
#' @export
plotCorrelationMatrix <- function(corrmat, legend = TRUE, style = "blue") {
  
  # flip the columns
  corrmat <- corrmat[ , ncol(corrmat):1 ]
  
  corrmat <- reshape2::melt(corrmat)
  
  plot <- ggplot(data = corrmat, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white")+
    #theme_minimal()+
    #theme(axis.text.x = element_text(angle = 45, vjust = 1,
    #                                 size = 12, hjust = 1))+
    coord_fixed() +
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      #axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      #panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=0.5)
      ) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) 
  
  if (style == 'blue') { 
    plot <- plot + scale_fill_gradient2(low = "white", high = "deepskyblue4", mid = "deepskyblue", 
                         midpoint = 0.5, limit = c(0,1), space = "Lab",
                         name="Correlation") 
  } else {
    plot <- plot + scale_fill_gradient2(low = "white", high = "black", mid = "grey", 
                                midpoint = 0.5, limit = c(0,1), space = "Lab",
                                name="Correlation") 
  }
  
  if (!legend) {
     plot <- plot + theme(legend.position="none") 
  }
  return(plot)
}
