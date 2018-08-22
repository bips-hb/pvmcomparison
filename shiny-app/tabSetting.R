tabSetting <- tabPanel("Methods", 
                       withMathJax(),
                       sidebarLayout(
                         sidebarPanel(
                           shinyWidgets::sliderTextInput(
                             "or_setting",
                             label = "Mean Odds Ratio (OR)",
                             choices = c(1.5, 3, 5),
                             animate = TRUE
                           ),
                           
                           hr(),
                           
                           radioButtons(
                             "measure_setting",
                             label = "Measure",
                             choiceNames = list(
                               "\\(\\#\\ reports\\)", 
                               "\\(\\chi^2\\)",
                               "\\(\\chi^2_{Yates}\\)",
                               "\\(EBGM\\)",
                               "\\(EB_{025}\\)",
                               "\\(EB_{05}\\)",
                               "\\(IC^{alternative}\\)",
                               "\\(IC^{alternative}_{025}\\)",
                               "\\(IC^{alternative}_{05}\\)",
                               "\\(IC^{original}\\)",
                               "\\(IC^{original}_{025}\\)",
                               "\\(IC^{original}_{05}\\)",
                               "\\(\\Lambda_{binomial}\\)",
                               "\\(LASSO\\)",
                               "\\(RFET\\)",
                               "\\(midRFET\\)",
                               "\\(p_{Poisson}\\)",
                               "\\(PRR\\)",
                               "\\(PRR_{025}\\)",
                               "\\(PRR_{05}\\)",
                               "\\(Q\\)",
                               "\\(Q_{025}\\)",
                               "\\(Q_{05}\\)",
                               "\\(ROR\\)",
                               "\\(ROR_{025}\\)",
                               "\\(ROR_{05}\\)",
                               "\\(RRR\\)"
                             ),
                             choiceValues = list(
                               "a",
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
                               "RFET",
                               "midRFET", 
                               "ppoisson", 
                               "PRR", 
                               "PRR025",
                               "PRR05", 
                               "Q", 
                               "Q025", 
                               "Q05", 
                               "ROR", 
                               "ROR025", 
                               "ROR05", 
                               "RRR"
                             ),
                             selected = "a"
                           )
                           
                           ),
                         mainPanel(
                           plotOutput("boxplotSetting")
                           )
                       ))


# method             method_label
# 1               a                # reports
# 2            chi2                $\\chi^2$
#   3       chi2Yates        $\\chi^2_{Yates}$
#   4             GPS                     EBGM
# 5          GPS025               $EB_{025}$
#   6           GPS05                $EB_{05}$
#   7           ICAlt       $IC^{alternative}$
#   8        ICAlt025 $IC^{alternative}_{025}$
#   9         ICAlt05  $IC^{alternative}_{05}$
#   10         ICOrig          $IC^{original}$
#   11      ICOrig025    $IC^{original}_{025}$
#   12       ICOrig05     $IC^{original}_{05}$
#   13      lbinomial    $\\Lambda_{binomial}$
#   14 highest_lambda                    LASSO
# 15        midRFET                  midRFET
# 16       ppoisson            $p_{Poisson}$
#   17            PRR                      PRR
# 18         PRR025              $PRR_{025}$
#   19          PRR05               $PRR_{05}$
#   20              Q                      $Q$
#   21           Q025                $Q_{025}$
#   22            Q05                 $Q_{05}$
#   23           RFET                     RFET
# 24            ROR                      ROR
# 25         ROR025              $ROR_{025}$
#   26          ROR05               $ROR_{05}$
#   27            RRR                      RRR