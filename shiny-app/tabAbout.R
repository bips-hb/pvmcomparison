tabAbout <- tabPanel("About", 
  p("For more information, see:"),
  p(
    strong("Adverse Event Discovery for Spontaneous Reporting Systems: 
                          A Systematic Comparison"),
    br(),
    em("L.J. Dijkstra, M. Garling, R. Foraita & I. Pigeot"),
    br(),
    br(),
    em("To be submitted (2018)")),
  
  h3("Software"),
  
  p("The R code used for this project is publicly available under the
                            GPL-3 license and can be found at: "), 
  tags$ul(
    tags$li(
      a("SRSim", href="https://github.com/bips-hb/srsim"),
      ": a spontaneous reporting data simulator"
    ),
    tags$li(
      a("PVM", href="https://github.com/bips-hb/pvm"),
      ": an R package containing the implementation of all the 27 methods used here"
    ),
    tags$li(
      a("pvmcomparison", href="https://github.com/bips-hb/pvmcomparison"),
      " contains all the code for generating the results presented here"
    )
  ), 
  
  h3("Conflict of Interest"), 
  
  p("The authors declare that there are no conflicts of interest"),
  
  h3("Contact"),
  
  p("Louis Dijkstra",
    br(),
    "Leibniz Institute for Prevention Research and Epidemiology - BIPS",
    br(),
    "Department Biometry & Data Management",
    br(),
    "E-mail:", 
    a("dijkstra@leibniz-bips.de"),
    br(),
    a("http://www.leibniz-bips.de/en/")
    )
    
)