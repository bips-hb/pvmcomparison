tabMain <- tabPanel("Main", 
                    withMathJax(),
                    sidebarLayout(
                      sidebarPanel(
                        p(strong("Adverse Drug Reaction or Innocent Bystander? A Systematic Comparison of 
                                  Statistical Discovery Methods for Spontaneous Reporting Systems"),
                          p("L.J. Dijkstra, M. Garling, R. Foraita & I. Pigeot"),
                          em("Pharmacoepidemiology and Drug Safety (2020)"),
                          p(strong("DOI:"),"10.1002/PDS.4970"),
                          hr(),
                          p("In order to see all results for a particular parameter 
                            setting, go to the tab ", 
                            em("Ranking")),
                          br(),
                          p("In order to see all results for one particular method, 
                            go to the tab ",
                            em("Methods")),
                          hr(), 
                          h4("Software"),
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
                          
                          hr(),
                          
                          h4("Conflict of Interest"), 
                          
                          p("The authors declare that there are no conflicts of interest"),
                          
                          hr(), 
                          
                          h4("Contact"),
                          
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
                      )
                      ,
                      mainPanel(
                        h1("Comparing Methods for Pharmacovigilance"), 
                        p("Spontaneous reporting systems (SRSs) are often used in the 
                          field of pharmacovigilance to discover previously unknown 
                          adverse drug reactions (ADRs). Over the last three decades a plethora
                          of statistical methods have been proposed to aid in the detection
                          of drug-ADR associations. Here we present the results of a comprehensive
                          simulation study in which we compare 24 different measures that have 
                          been proposed or used in the literature."),
                        
                        h3("Methods"),
                        p("The following 27 measures are considered:"),
                        
                        tags$ul(
                          tags$li("\\(\\#\\ reports\\) - the number of reports that contain both the drug and AE of interest"), 
                          tags$li("\\(\\chi^2\\) - chi-squared test"),
                          tags$li("\\(\\chi^2_{Yates}\\) - chi-squared test with Yates' continuity correction"),
                          tags$li("\\(EBGM\\) - the Gamma Poisson shrinker"),
                          tags$li("\\(EB_{025}\\) - the lower bound of the 97.5% credible interval of the \\(EBGM\\)"),
                          tags$li("\\(EB_{05}\\) - the lower bound of the 95% credible interval of the \\(EBGM\\)"),
                          tags$li("\\(IC^{alternative}\\) - The Bayesian confidence propagation neural network (BCPNN) as proposed by Noren et. al. (2006)"),
                          tags$li("\\(IC^{alternative}_{025}\\) - the lower bound of the 97.5% credible interval of the \\(IC^{alternative}\\)"),
                          tags$li("\\(IC^{alternative}_{05}\\) - the lower bound of the 95% credible interval of the \\(IC^{alternative}\\)"),
                          tags$li("\\(IC^{original}\\) - The Bayesian confidence propagation neural network (BCPNN) as proposed by Bate et. al. (1998)"),
                          tags$li("\\(IC^{original}_{025}\\) - the lower bound of the 97.5% credible interval of the \\(IC^{original}\\)"),
                          tags$li("\\(IC^{original}_{05}\\)  - the lower bound of the 95% credible interval of the \\(IC^{original}\\)"),
                          tags$li("\\(\\Lambda_{binomial}\\) - likelihood ratio test assuming a binomial distribution for the 2 x 2 tables"),
                          tags$li("\\(LASSO\\) - a sparse regression method"),
                          tags$li("\\(RFET\\) - reporting Fisher's exact test"), 
                          tags$li("\\(midRFET\\) - reporting Fisher's exact test with a mid-P-value correction"),
                          tags$li("\\(p_{Poisson}\\) - test of the Poisson mean"),
                          tags$li("\\(PRR\\) - proportional relative risk"),
                          tags$li("\\(PRR_{025}\\) - lower bound of the 97.5% confidence interval of the \\(PRR\\)"),
                          tags$li("\\(PRR_{05}\\) - lower bound of the 95% confidence interval of the \\(PRR\\)"),
                          tags$li("\\(ROR\\) - reporting odds ratio"),
                          tags$li("\\(ROR_{025}\\) - lower bound of the 97.5% confidence interval of the \\(ROR\\)"),
                          tags$li("\\(ROR_{05}\\) - lower bound of the 95% confidence interval of the \\(ROR\\)"),
                          tags$li("\\(RRR\\) - relative report rate")
                        ),
                        
                        p("Please, see the paper for more information on the simulation set-up and how the performance
                          of these measures is assessed.")
                        
                      ) 
                      
                    )
)
