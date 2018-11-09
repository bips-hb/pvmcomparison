tabMain <- tabPanel("Main", 
                    withMathJax(),
                    sidebarLayout(
                      sidebarPanel(
                        p(strong("Adverse Event or Innocent Bystander? A Systematic Comparison of 
                                  Statistical Discovery Methods for Spontaneous Reporting Systems"),
                          br(),
                          em("L.J. Dijkstra, M. Garling, R. Foraita & I. Pigeot"),
                          br(),
                          br(),
                          em("Submitted (2018)"),
                          hr(),
                          p("In order to see all results for a particular parameter 
                            setting, go to the tab ", 
                            em("Ranking")),
                          br(),
                          p("In order to see all results for one particular method, 
                            go to the tab ",
                            em("Methods")),
                          hr(), 
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
                          )
                            
                        )
                      )
                      ,
                      mainPanel(
                        h1("Comparing Methods for Pharmacovigilance"), 
                        p("Spontaneous reporting systems (SRSs) are often used in the 
                          field of pharmacovigilance to discover previously unknown 
                          adverse events (AEs). Over the last three decades a plethora
                          of statistical methods have been proposed to aid in the detection
                          of drug-AE associations. Here we present the results of a comprehensive
                          simulation study in which we compare 27 different measures that have 
                          been proposed or used in the literature."),
                        
                        h3("Methods"),
                        p("We cast the net wide and consider 27 different measures, ranging 
                           from simple disproportionality measures (e.g., the reporting odds
                           ratio), hypothesis tests (e.g., chi-squared), Bayesian shrinkage 
                           methods (e.g., the Bayesian confidence propagation neural network - BCPNN),
                           to sparse regression (the LASSO). 
                           The following 27 measures are considered:"),
                        
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
                          tags$li("\\(Q\\) - Yule's Q"),
                          tags$li("\\(Q_{025}\\) - lower bound of the 97.5% confidence interval of \\(Q\\)"),
                          tags$li("\\(Q_{05}\\) - lower bound of the 95% confidence interval of \\(Q\\)"),
                          tags$li("\\(ROR\\) - reporting odds ratio"),
                          tags$li("\\(ROR_{025}\\) - lower bound of the 97.5% confidence interval of the \\(ROR\\)"),
                          tags$li("\\(ROR_{05}\\) - lower bound of the 95% confidence interval of the \\(ROR\\)"),
                          tags$li("\\(RRR\\) - relative report rate")
                        ),
                        
                        p("Please, see the paper for more information on the individual measures."),
                        
                        
                        
                        h3("Simulation Set-Up"),
                        
                        p("Let \\(m\\) and \\(n\\) be the number of drugs and AEs that are in the SRS. 
                          Each report sent to a SRS can then be represented by a binary vector:"),
                        
                        p("\\( (X_1, X_2, ..., X_m; Y_1, Y_2, ..., Y_n) \\)", align = "center"),
                        
                        p("where \\(X_i\\) is 1 when the \\(i\\)-th drug is on the report (and 0 otherwise), 
                          and, similary, \\(Y_j\\) is 1 when the \\(j\\)-th AE is on the report (and 0 otherwise). 
                          A spontaneous reporting data set can, thus, be seen as the result of repeatedly drawing 
                          from a multivariate Bernoulli distribution. In our simulation set-up the number of drugs
                          and AEs are both fixed at 500, i.e., \\(m = n = 500\\)."),
                        
                        p("The relationships between the variables are represented by a directed 
                          acyclic graph (DAG):"),
                        
                        shiny::img(src="figure2.png", align = "center", width = "100%"),
                        
                        p("The top row represents the 500 drugs, the lower row the 500 AEs. An 
                          arrow pointing from node A to node B reflects that the probability of 
                          B being 1 is influenced by the value of A. Note that the first 250 drugs,
                          \\(X_1\\) to \\(X_{250}\\), are connected to the first 250 AEs, 
                          \\(Y_1\\) to \\(Y_{250}\\). These form the associated drug-AE pairs that 
                          each of the methods tries to detect. In addition, the first 250 drugs are 
                          connected to the last 250 drugs. They form the innocent bystanders, since 
                          they are prescribed in combination with the drug they are connected to, 
                          and, thus, might confound the method to 'think' that they are the ones
                          actually associated with the AE. For example, \\(X_{251}\\) is the 
                          innocent bystander for \\(X_1\\) and \\(Y_1\\). In our simulation we set 
                          the number of innocent bystanders to either 0 (no innocent bystander effect),
                          125 or 250."),
                        
                        p("The marginal probabilities of the drugs (that are no innocent 
                          bystanders) and the AEs are drawn form a Beta distribution with the 
                          shape and rate parameters set to 1 and 20, respectively. The fact that 
                          an innocent bystander tends to be prescribed in combination with the other
                          drug can be represented by setting the conditional probability"),
                        
                        p("\\(P(X_{bystander} = 1 | X = 1) = \\gamma\\)", align = "center"),
                        
                        p("where \\(X_{bystander}\\) is the innocent bystander and \\(X\\) denotes 
                          the drug that is the actual cause of the AE. The parameter \\(\\gamma\\) is 
                          set to .5, .75 or .9 for all innocent bystanders. The conditional probability
                          \\(P(X_{bystander} = 1 | X = 0)\\) is drawn from the same Beta distribution 
                          as before."), 
                        
                        p("The relationship between the AE, \\(Y\\), and a drug, \\(X\\), is expressed 
                          using the logistic regression model"), 
                        
                        p("\\( logit(P(Y = 1 | X = x) = \\beta_0 + log(OR) * x\\)", align = "center"),
                        
                        p("where \\(\\beta_0\\) is the intercept and \\(OR\\) is the odds ratio. 
                          The OR is drawn from a truncated Normal distribution with unit variance and
                          a mean of either 1.5 (weak effect), 3 (medium) or 5 (strong)."),
                        
                        p("Not every binary vector is a valid report, since each report should 
                          at least contain one drug and one AE (otherwise it would never be submitted). 
                          In case the drawn binary vector does not satisfy these conditions, it is 
                          discarded and not added to the SRS."),
                        
                        p("The parameters used for the simulations are: "),
                        
                        tags$ul(
                          tags$li("Number of reports: 50,000"),
                          tags$li("Number of drugs: \\(m\\) = 500"),
                          tags$li("Number of AEs: \\(n\\) = 500"), 
                          tags$li("Number of innocent bystanders: 0, 125 or 500"), 
                          tags$li("Conditional probability bystander, \\(\\gamma\\): .5, .75 or .9"), 
                          tags$li("Mean OR associated drug-AE pairs: 1.5, 3 or 5")
                        ), 
                        
                        p("There are, thus, 21 parameter settings in total. For each setting, 
                          we generated 50 SRSs, resulting in a total of 1,050 distinct 
                          spontaneous reporting data sets."), 
                        
                        
                        h3("Assessing the Performance"),
                        
                        p("Each of the 27 measures are applied to each of the 1,050 simulated SRSs
                          (see the previous section). Each time the precision-recall curve (PRC) is 
                          created that shows the changes in precision and recall when the treshold 
                          is varied from most stringent to most relaxed. The area under this curve 
                          is used to assess the methods' overall capability to distinguish between
                          associated and not-associated drug-AE pairs. The choice to use the PRC 
                          rather than the more common receiver operating curve (ROC) is due to the 
                          highly imbalanced nature of the data.")
                      ) 
                      
                    )
)
