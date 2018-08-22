library(shiny)
library(shinyWidgets)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(gtable)
library(gridExtra)
library(grid)

# Load the data ---
source("loadData.R")

# Load the plotting functionality ---
source("plot.R")

# Load the individual tabs ---
source("tabMain.R")
source("tabRanking.R")
source("tabSetting.R")
source("tabAbout.R")

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



# Define UI ----
ui <- fluidPage(navbarPage("PV Comparison",
                           tabMain,
                           tabRanking,
                           tabSetting,
                           tabAbout))

# Define server logic ----
server <- function(input, output) {
  observe({
    toggleState(id = "gamma",
                condition = input$n_innocent_bystanders != 0)
  })
  
  output$boxplot <- renderPlot({
    
    data <- results %>% dplyr::filter(
      n_innocent_bystanders == as.integer(input$n_innocent_bystanders),
      theta == as.double(input$or))
    
    if (input$n_innocent_bystanders != 0) { 
      data <- data %>% dplyr::filter(bystander_prob == as.double(input$gamma))
    }
    
    if (input$sorted == "mean") {
      data <- data %>% group_by(method) %>% 
        mutate(score = mean(PRCAUC, na.rm = TRUE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(score) 
    } else if (input$sorted == "median") { 
      data <- data %>% group_by(method) %>% 
        mutate(score = median(PRCAUC, na.rm = TRUE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(score) 
    } else { 
      data <- data %>% dplyr::mutate(score = row_number())  
    }
    
    ylim <- NULL
    if (input$xlim_fixed) { 
      ylim <- c(0,1) 
    }
    
    if (input$n_innocent_bystanders == 0) {
      title <- TeX(sprintf("OR$\\approx %g$, no innocent bystanders", input$or)) 
    } else { 
      title <- TeX(sprintf("OR$\\approx %g$, %d innocent bystanders, $\\gamma = %g$", 
                           input$or, input$n_innocent_bystanders, input$gamma))  
    }
    plotBoxplot(data, ylim = ylim, title = title, measure = "PRCAUC") 
  })
  
  
  output$boxplotSetting <- renderPlot({
    
    method <- input$measure_setting
    method_name <- labels[labels$method == method,]$method_label  
      
    if (input$or_setting == 5.0) {
      title <-
        TeX(sprintf("%s - strong assocation ($OR \\approx 5$)", method_name))
    } else if (input$or_setting == 3.0) {
      title <-
        TeX(sprintf("%s - medium assocation ($OR \\approx 3$)", method_name))
    } else if (input$or_setting == 1.5) {
      title <-
        TeX(sprintf("%s - weak assocation ($OR \\approx 1.5$)", method_name))
    }
    
    data <- results %>% dplyr::filter(
      method == input$measure_setting,
      theta == as.double(input$or_setting))
    
    measure <- "PRCAUC"
    ylim <- c(max(0.0, min(data[[measure]], na.rm = TRUE) - 0.01),
              min(1.0, max(data[[measure]], na.rm = TRUE) + 0.01))
    
    plotSettings(data, title = title, ylim = ylim) 
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)