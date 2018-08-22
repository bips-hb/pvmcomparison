tabRanking <- tabPanel("Ranking",
                       withMathJax(),
                       sidebarLayout(
                         sidebarPanel(

                           shinyWidgets::sliderTextInput(
                             "or",
                             label = "Mean Odds Ratio (OR)",
                             choices = c(1.5, 3, 5),
                             animate = TRUE
                           ),
                           
                           hr(),

                           useShinyjs(),
                           
                           sliderInput(
                             "n_innocent_bystanders",
                             label = "Number of innocent bystanders",
                             min = 0,
                             max = 250,
                             value = 0,
                             step = 125,
                             animate = TRUE
                           ),

                           shinyWidgets::sliderTextInput(
                             "gamma",
                             label = "Conditional probability \\((\\gamma)\\)",
                             choices = c(.5, .75, .9),
                             animate = TRUE
                           ),
                            
                           hr(),
                           
                           radioButtons(
                             "sorted",
                             label = "How do you want to sort the results?",
                             choices = list(
                               "Alphabetically" = "fixed",
                               "By their mean" = "mean",
                               "By their median" = "median"
                             ),
                             selected = "mean"
                           ),
                           
                           hr(),
                           
                           checkboxInput("xlim_fixed", "X-axis fixed to [0,1]", value = TRUE)
                           
                           ),
                         mainPanel(
                            plotOutput("boxplot", height = 700)
                           ))
                       
                       )
