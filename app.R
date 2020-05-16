library(shiny)
library(lifecontingencies)
library(tidyverse)
library(actuar)

source("functions.R", local = TRUE)

# UI----
ui <- fluidPage(

  navbarPage("Actuarial Tools",
             
             tabPanel("Home",
                      h2("Welcome to Actuarial Tools!"),
                      p("Here is a list of the currently available tools:"),
                      h3("Annuity Calculators"),
                      p(HTML(paste(
                        tags$ul(
                          tags$li("Annuity Present Value"),
                          tags$li("Increasing Annuity PV"),
                          tags$li("Decreasing Annuity PV")
                        )
                      ))),
                      h3("Probability Distributions"),
                      p(HTML(paste(
                        tags$ul(
                          tags$li("Normal Distribution"),
                          tags$li("Binomial Distribution"),
                          tags$li("Gamma Distribution"),
                          tags$li("Poisson Distribution"),
                          tags$li("Pareto Type-2 Distribution"),
                        )
                      ))),
                      h3("Life Contingencies"),
                      p("Note: These tools use the AM92 table from the ", a(href="https://cran.r-project.org/web/packages/lifecontingencies/index.html","lifecontingencies"), " package."),
                      p(HTML(paste(
                        tags$ul(
                          tags$li("Life Assurance PV"),
                          tags$li("Life Annuity PV")
                        )
                      )))
                      ),
             
             ## Annuity Calculators----
             navbarMenu("Annuity Calculators",
             ### Annuity----
             
              tabPanel("Annuity Present Value",
                       sidebarPanel(
                         numericInput("interest",label=h3("Interest rate (%)"), value = 4, min = 0),
                         numericInput("duration",label=h3("Duration (years)"), value = 5, min = 0),    
                         numericInput("defer",label=h3("Deferral period (years)"), value = 0, min = 0),
                         numericInput("pthly",label=h3("Payments per year"), value = 1, min = 1), 
                         radioButtons("type", label=h3("Arrears or advance payments"),
                          choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears"),
                       ), #sideBarPanel
                       mainPanel(
                         h2("Annuity Present Value Calculator"),
                         HTML(paste(
                           tags$table(
                             tags$tr(
                               tags$td(h4(tags$b("PV"))),
                               tags$td(h4(HTML("&emsp;"))),
                               tags$td(h4(textOutput("annuityPV")))
                             ),
                             tags$tr(
                               tags$td(h4(HTML(paste(tags$b("(1+i)",tags$sup("n")))))),
                               tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                               tags$td(h4(textOutput("compoundRate")))
                             ),
                             tags$tr(
                               tags$td(h4(HTML(paste(tags$b("v",tags$sup("n")))))),
                               tags$td(h4(HTML("&emsp;"))),
                               tags$td(h4(textOutput("discountFactor")))
                             ),
                             tags$tr(
                               tags$td(h4(HTML(paste(tags$b("d"))))),
                               tags$td(h4(HTML("&emsp;"))),
                               tags$td(h4(textOutput("annualEffective")))
                             ),
                             tags$tr(
                               tags$td(h4(HTML(paste(tags$b(HTML("&delta;")))))),
                               tags$td(h4(HTML("&emsp;"))),
                               tags$td(h4(textOutput("forceOfInterest")))
                             ),
                             tags$tr(
                               tags$td(h4(HTML(paste(tags$b("i",tags$sup("(p)")))))),
                               tags$td(h4(HTML("&emsp;"))),
                               tags$td(h4(textOutput("interestPthly")))
                             ),
                             tags$tr(
                               tags$td(h4(HTML(paste(tags$b("d",tags$sup("(p)")))))),
                               tags$td(h4(HTML("&emsp;"))),
                               tags$td(h4(textOutput("effectivePthly")))
                             )
                           ) #tagstable
                         )) #HTML
                       ) #mainPanel Annuity
                       ), #tabPanel Annuity
             
             
             ### Increasing----
             tabPanel("Increasing Annuity PV",
                      sidebarPanel(
                        numericInput("interest2",label=h3("Interest rate (%)"), value = 4, min = 0),
                        numericInput("duration2",label=h3("Duration (n)"), value = 5, min = 1),    
                        radioButtons("type2", label=h3("Arrears or advance payments"),
                          choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears"),
                      ), #sidebarPanel
                      mainPanel(
                        h2("Increasing Annuity PV Calculator"),
                        HTML(paste(
                          tags$table(
                            tags$tr(
                              tags$td(h4(tags$b("PV"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("increaseAnnuityPV")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b("(1+i)",tags$sup("n")))))),
                              tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                              tags$td(h4(textOutput("compoundRate2")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b("v",tags$sup("n")))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("discountFactor2")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b("d"))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("annualEffective2")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b(HTML("&delta;")))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("forceOfInterest2")))
                            )
                          ) #tagstable
                        )) #HTML
                      ) #mainPanel
                      ), #tabPanel Increasing
             
             ### Decreasing ----
             tabPanel("Decreasing Annuity PV",
                      sidebarPanel(
                        numericInput("interest3",label=h3("Interest rate (%)"), value = 4, min = 0),
                        numericInput("duration3",label=h3("Duration (n)"), value = 5, min = 1),    
                        radioButtons("type3", label=h3("Arrears or advance payments"),
                                     choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears"),
                      ), #sidebarPanel
                      mainPanel(
                        h2("Decreasing Annuity PV Calculator"),
                        HTML(paste(
                          tags$table(
                            tags$tr(
                              tags$td(h4(tags$b("PV"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("decreaseAnnuityPV")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b("(1+i)",tags$sup("n")))))),
                              tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                              tags$td(h4(textOutput("compoundRate3")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b("v",tags$sup("n")))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("discountFactor3")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b("d"))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("annualEffective3")))
                            ),
                            tags$tr(
                              tags$td(h4(HTML(paste(tags$b(HTML("&delta;")))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("forceOfInterest3")))
                            )
                          ) #tagstable
                        )) #HTML
                      ) #mainPanel
             ) #tabPanel Decreasing

    
            
             ), #navBarMenu - Annuity Calculator
             
             ## Probability----
             navbarMenu("Probability Distributions",
             
             ### Normal----
                        tabPanel("Normal Distribution",
                                 sidebarPanel(
                                   numericInput("normMean",label=h3("Mean"), value = 0),
                                   numericInput("normVar",label=h3("Variance"), value = 1, min = 0),    
                                   numericInput("normX",label=h3("x"), value = 0)
                                 ),
                                 mainPanel(
                                   h2("Normal Distribution"),
                                   plotOutput("normPlot",width = "500px"),
                                   tags$table(
                                     tags$tr(
                                       tags$td(h4(tags$b(HTML(paste("&sigma;"))))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("normSD")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b("P[X<=x]"))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("normLess")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b("P[X>x]"))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("normGreat")))
                                     )
                                   )
                                 ) #main
                                 ), #tab Panel Normal
             ### Binomial----
                        tabPanel("Binomial Distribution",
                                 sidebarPanel(
                                   numericInput("binN",label=h3("n"), value = 10, min = 0),
                                   numericInput("binP",label=h3("p"), value = 0.5, min = 0, max = 1, step = 0.05),    
                                   numericInput("binX",label=h3("x"), value = 5, min=0)
                                 ),
                                 mainPanel(
                                   h2("Binomial Distribution"),
                                   plotOutput("binPlot", width = "500px"),
                                   HTML(paste(
                                     tags$table(
                                     tags$tr(
                                       tags$td(h4(tags$b("E[X]"))),
                                       tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                                       tags$td(h4(textOutput("binMean")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b("Var[X]"))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("binVar")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b(HTML(paste("&sigma;",tags$sub("X")))))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("binSD")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b("P[X=x]"))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("binEqual")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b("P[X<=x]"))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("binLess")))
                                     ),
                                     tags$tr(
                                       tags$td(h4(tags$b("P[X>x]"))),
                                       tags$td(h4(HTML("&emsp;"))),
                                       tags$td(h4(textOutput("binGreat")))
                                     )
                                     ) #table
                                   )) #HTML Paste
                                 )
                                 ), #tabPanel Bin
             ### Gamma ----
             tabPanel("Gamma Distribution",
                      sidebarPanel(
                        numericInput("gammaA",label=h3(HTML(paste("&alpha;"))), value = 1, min = 0, step = 0.5),
                        numericInput("gammaB",label=h3(HTML(paste("&beta;"))), value = 1, min = 0, step = 0.5),    
                        numericInput("gammaX",label=h3("x"), value = 1, min=0, step = 0.5)
                      ),
                      mainPanel(
                        h2("Gamma Distribution"),
                        plotOutput("gammaPlot", width = "500px"),
                        HTML(paste(
                          tags$table(
                            tags$tr(
                              tags$td(h4(tags$b("E[X]"))),
                              tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                              tags$td(h4(textOutput("gammaMean")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("Var"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("gammaVar")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b(HTML(paste("&sigma;"))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("gammaSD")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X<=x]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("gammaLess")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X>]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("gammaGreat")))
                            )
                          )
                        ))
                      )
                      ), #tabPanel
             ### Poisson----
             tabPanel("Poisson Distribution",
                      sidebarPanel(
                        numericInput("poX",label=h3("x"), value = 1, min = 1),
                        numericInput("poK",label=h3(HTML(paste("&lambda;"))), value = 1, min = 1)
                      ),
                      mainPanel(
                        h2("Poisson Distribution"),
                        plotOutput("poPlot", width = "500px"),
                        HTML(paste(
                          tags$table(
                            tags$tr(
                              tags$td(h4(tags$b("E[X]"))),
                              tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                              tags$td(h4(textOutput("poMean")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("Var[X]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("poVar")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b(HTML(paste("&sigma;",tags$sub("X")))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("poSD")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X=x]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("poEqual")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X<=x]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("poLess")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X>x]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("poGreat")))
                            )
                          )
                        ))
                      ) #main
                      ), #tabPanel
             ### Pareto Type 2 ----
             tabPanel("Pareto Type-2 Distribution",
                      sidebarPanel(
                        numericInput("pareto2X",label=h3("x"), value = 1, min = 0, step = 0.1),
                        numericInput("pareto2A",label=h3(HTML(paste("&alpha;"))), value = 3, min = 0, step = 0.1),
                        numericInput("pareto2K",label=h3(HTML(paste("&lambda;"))), value = 1, min = 0, step = 0.1)
                      ),
                      mainPanel(
                        h2("Pareto Type-2 Distribution"),
                        plotOutput("pareto2Plot", width = "500px"),
                        HTML(paste(
                          tags$table(
                            tags$tr(
                              tags$td(h4(tags$b("E[X]"))),
                              tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                              tags$td(h4(textOutput("pareto2Mean")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("Var[X]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("pareto2Var")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b(HTML(paste("&sigma;",tags$sub("X")))))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("pareto2SD")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X<=x]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("pareto2Less")))
                            ),
                            tags$tr(
                              tags$td(h4(tags$b("P[X>x]"))),
                              tags$td(h4(HTML("&emsp;"))),
                              tags$td(h4(textOutput("pareto2Great")))
                            )
                          )
                        ))
                      ) #main
             )
                        ), #navbarMenu
             ## Life Contingencies----
             navbarMenu("Life Contingencies",
             ### Life Assurance ----
                        tabPanel("Life Assurance PV",
                                 sidebarPanel(
                                   numericInput("lifeInterest",label=h3("Interest rate (%)"), value = 4, min = 0),
                                   numericInput("lifeAge",label=h3("Age (years)"), value = 40, min = 0, max = 109),
                                   numericInput("lifeDuration",label=h3("Duration (years)"), value = 5, min = 1),    
                                   numericInput("lifeDefer",label=h3("Deferral period (years)"), value = 0, min = 0),
                                  ), #sideBarPanel
                                 mainPanel(
                                   h2(" Life Assurance PV Calculator"),
                                   HTML(paste(
                                     tags$table(
                                       tags$tr(
                                         tags$td(h4(tags$b("PV"))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("lifePV")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("(1+i)",tags$sup("n")))))),
                                         tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                                         tags$td(h4(textOutput("lifeRate")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("v",tags$sup("n")))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("lifeFactor")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("d"))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("lifeEffective")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b(HTML("&delta;")))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("lifeForce")))
                                       )
                                     )))
                                 ) #mainPanel
                                 ), #tabPanel
             ### Life Annuity ----
                        tabPanel("Life Annuity PV",
                                 sidebarPanel(
                                   numericInput("life2Interest",label=h3("Interest rate (%)"), value = 4, min = 0),
                                   numericInput("life2Age",label=h3("Age (years)"), value = 40, min = 0, max = 109),
                                   numericInput("life2Duration",label=h3("Duration (years)"), value = 5, min = 1),    
                                   numericInput("life2Defer",label=h3("Deferral period (years)"), value = 0, min = 0),
                                   numericInput("life2Pthly",label=h3("Payments per year"), value = 1, min = 1), 
                                   radioButtons("life2Type", label=h3("Arrears or advance payments"),
                                                choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears")
                                 ), #sideBarPanel
                                 mainPanel(
                                   h2(" Life Annuity PV Calculator"),
                                   HTML(paste(
                                     tags$table(
                                       tags$tr(
                                         tags$td(h4(tags$b("PV"))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2PV")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("(1+i)",tags$sup("n")))))),
                                         tags$td(h4(HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2Rate")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("v",tags$sup("n")))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2Factor")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("d"))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2Effective")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b(HTML("&delta;")))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2Force")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("i",tags$sup("(p)")))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2IntPthly")))
                                       ),
                                       tags$tr(
                                         tags$td(h4(HTML(paste(tags$b("d",tags$sup("(p)")))))),
                                         tags$td(h4(HTML("&emsp;"))),
                                         tags$td(h4(textOutput("life2EffectivePthly")))
                                       )
                                     )))
                                 ) #mainPanel
                                 )
                        ), #navbarMenu life,
             tabPanel("About",
                      h5("by A. Wan"),
                      h5(a(href="github.com/cfaw1","https://github.com/cfaw1")),
                      h5("Packages used"),
                      p(HTML(paste(
                        tags$ul(
                          tags$li(a(href="https://cran.r-project.org/web/packages/lifecontingencies/index.html","lifecontingencies")),
                          tags$li(a(href="https://cran.r-project.org/web/packages/actuar/index.html","actuar")),
                          tags$li(a(href="https://cran.r-project.org/web/packages/tidyverse/index.html","tidyverse"))
                        )
                      ))),
                      h5("Created with ", a(href="https://cran.r-project.org/web/packages/shiny/index.html","shiny"))
             )
  ) #navbarPage
  
) #fluidpage

# Server----
server <- function(input, output) {
data(AM92Lt)
  ## Annuity----
  output$annuityPV <- renderText(
    formatC(
      annuity(input$interest/100, input$duration, input$defer, input$pthly, input$type),
      digits = 4,
      format = "f"
    )
  )
  
  output$compoundRate <- renderText(
    formatC(
      (1 + input$interest/100) ^ (input$duration + input$defer),
      digits = 6,
      format = "f"
    )
  )
  
  output$discountFactor <- renderText(
    formatC(
      discountRate(input$interest/100) ^ (input$duration + input$defer),
      digits = 6,
      format = "f"
    )
  )
  
  output$annualEffective <- renderText(
    formatC(
      ((input$interest/100)/(1 + input$interest/100)),
      digits = 6,
      format = "f"
    )
  )
  
  output$forceOfInterest <- renderText(
    formatC(
      forceOfInterest(input$interest/100),
      digits = 6,
      format = "f"
    )
  ) 
  
  output$interestPthly <- renderText(
    formatC(
      interestPthly(input$interest/100,input$pthly),
      digits = 6,
      format = "f"
    )
  ) 
  
  output$effectivePthly <- renderText(
    formatC(
      effectivePthly(input$interest/100,input$pthly),
      digits = 6,
      format = "f"
    )
  ) 

  ## Increasing Annuity----
  output$increaseAnnuityPV <- renderText(
    formatC(
      increasingAnnuity(input$interest2/100, input$duration2, input$type2),
      digits = 4,
      format = "f"
    )
  )
  
  output$compoundRate2 <- renderText(
    formatC(
      (1 + input$interest2/100) ^ input$duration2,
      digits = 6,
      format = "f"
    )
  )
  
  output$discountFactor2 <- renderText(
    formatC(
      discountRate(input$interest2/100) ^ input$duration2,
      digits = 6,
      format = "f"
    )
  )
  
  output$annualEffective2 <- renderText(
    formatC(
      ((input$interest2/100)/(1 + input$interest2/100)),
      digits = 6,
      format = "f"
    )
  )
  
  output$forceOfInterest2 <- renderText(
    formatC(
      forceOfInterest(input$interest2/100),
      digits = 6,
      format = "f"
    )
  ) 
  ## Decreasing Annuity----
  
  output$decreaseAnnuityPV <- renderText(
    formatC(
      decreasingAnnuity(input$interest3/100, input$duration3, input$type3),
      digits = 4,
      format = "f"
    )
  )
  
  output$compoundRate3 <- renderText(
    formatC(
      (1 + input$interest3/100) ^ input$duration3,
      digits = 6,
      format = "f"
    )
  )
  
  output$discountFactor3 <- renderText(
    formatC(
      discountRate(input$interest3/100) ^ (input$duration3 + input$defer3),
      digits = 6,
      format = "f"
    )
  )
  
  output$annualEffective3 <- renderText(
    formatC(
      ((input$interest3/100)/(1 + input$interest3/100)),
      digits = 6,
      format = "f"
    )
  )
  
  output$forceOfInterest3 <- renderText(
    formatC(
      forceOfInterest(input$interest3/100),
      digits = 6,
      format = "f"
    )
  )
  ## Normal----
  
  output$normPlot <- renderPlot({
    p <- normplot(input$normX, input$normMean,input$normVar) 
    print(p)
  })
  
  output$normLess <- renderText(
    formatC(
      pnorm(input$normX,input$normMean,sqrt(input$normVar)),
      digits = 6,
      format = "f"
    )
  )
  
  output$normGreat <- renderText(
    formatC(
      1- pnorm(input$normX,input$normMean,sqrt(input$normVar)),
      digits = 6,
      format = "f"
    )
  )
  
  output$normSD <- renderText(
    formatC(
      sqrt(input$normVar),
      digits = 6,
      format = "f"
    )
  )
  
  ## Binomial----
  
  output$binPlot <- renderPlot({
    p <- binplot(input$binX, input$binN, input$binP)
    print(p)
  })
  
  output$binMean <- renderText(
    formatC(
      input$binN*input$binP,
      digits = 6,
      format = "f"
    )
  )
  
  output$binVar <- renderText(
    formatC(
      input$binN*input$binP*(1-input$binP),
      digits = 6,
      format = "f"
    )
  )
  
  output$binSD <- renderText(
    formatC(
      sqrt(input$binN*input$binP*(1-input$binP)),
      digits = 6,
      format = "f"
    )
  )
  
  output$binEqual <- renderText(
    formatC(
      dbinom(input$binX,input$binN,input$binP),
      digits = 6,
      format = "f"
    )
  )
  
  output$binLess <- renderText(
    formatC(
      pbinom(input$binX,input$binN,input$binP),
      digits = 6,
      format = "f"
    )
  )
  
  output$binGreat <- renderText(
    formatC(
      1-pbinom(input$binX,input$binN,input$binP),
      digits = 6,
      format = "f"
    )
  )
  
  ## Gamma----
  
  output$gammaPlot <- renderPlot({
    p <- gammaplot(input$gammaX,input$gammaA,input$gammaB)
    print(p)
  })
  
  output$gammaMean <- renderText(
    formatC(
      input$gammaA/input$gammaB,
      digits = 6,
      format = "f"
    )
  )
  
  output$gammaVar <- renderText(
    formatC(
      input$gammaA/input$gammaB^2,
      digits = 6,
      format = "f"
    )
  )
  
  output$gammaSD <- renderText(
    formatC(
      sqrt(input$gammaA/input$gammaB^2),
      digits = 6,
      format = "f"
    )
  )
  
  output$gammaLess <- renderText(
    formatC(
      pgamma(input$gammaX,input$gammaA,input$gammaB),
      digits = 6,
      format = "f"
    )
  )
  
  output$gammaGreat <- renderText(
    formatC(
      1-pgamma(input$gammaX,input$gammaA,input$gammaB),
      digits = 6,
      format = "f"
    )
  )
  
  ## Poisson ----
  output$poPlot <- renderPlot({
    p <- poplot(input$poX,input$poK)
    print(p)
  })
  
  output$poMean <- renderText(
    formatC(
      input$poK,
      digits = 6,
      format = "f"
    )
  )
  
  output$poVar <- renderText(
    formatC(
      input$poK,
      digits = 6,
      format = "f"
    )
  )
  
  output$poSD <- renderText(
    formatC(
      sqrt(input$poK),
      digits = 6,
      format = "f"
    )
  )
  
  output$poEqual <- renderText(
    formatC(
      dpois(input$poX,input$poK),
      digits = 6,
      format = "f"
    )
  )
  
  output$poLess <- renderText(
    formatC(
      ppois(input$poX,input$poK),
      digits = 6,
      format = "f"
    )
  )
  
  output$poGreat <- renderText(
    formatC(
      1-ppois(input$poX,input$poK),
      digits = 6,
      format = "f"
    )
  )
  
  ## Pareto2 ----
  
  output$pareto2Plot <- renderPlot({
    p <- pareto2plot(input$pareto2X,input$pareto2A,input$pareto2K)
    print(p)
  })
  
  output$pareto2Mean <- renderText(
    formatC(
      pareto2Mean(input$pareto2A,input$pareto2K),
      digits = 6,
      format = "f"
    )
  )
  
  output$pareto2Var <- renderText(
    formatC(
      pareto2Var(input$pareto2A,input$pareto2K),
      digits = 6,
      format = "f"
    )
  )
  
  output$pareto2SD <- renderText(
    formatC(
      sqrt(pareto2Var(input$pareto2A,input$pareto2K)),
      digits = 6,
      format = "f"
    )
  )
  
  output$pareto2Less <- renderText(
    formatC(
      ppareto2(input$pareto2X,input$pareto2A,input$pareto2K),
      digits = 6,
      format = "f"
    )
  )
  
  output$pareto2Great <- renderText(
    formatC(
      1-ppareto2(input$pareto2X,input$pareto2A,input$pareto2K),
      digits = 6,
      format = "f"
    )
  )
  
  ## Life Assurance ----

  output$lifePV <- renderText({
    if(input$lifeAge + input$lifeDuration > 109) stop("Maximum lifetime exceeded")
    formatC(
      Axn(AM92Lt, input$lifeAge, input$lifeDuration, input$lifeInterest/100, input$lifeDefer),
      digits = 4,
      format = "f"
    )
  })
  
  output$lifeRate <- renderText(
    formatC(
      (1 + input$lifeInterest/100) ^ (input$lifeDuration + input$lifeDefer),
      digits = 6,
      format = "f"
    )
  )
  
  output$lifeFactor <- renderText(
    formatC(
      discountRate(input$lifeInterest/100) ^ (input$lifeDuration + input$lifeDefer),
      digits = 6,
      format = "f"
    )
  )
  
  output$lifeEffective <- renderText(
    formatC(
      annualEffective(input$lifeInterest/100),
      digits = 6,
      format = "f"
    )
  )
  
  output$lifeForce <- renderText(
    formatC(
      forceOfInterest(input$lifeInterest/100),
      digits = 6,
      format = "f"
    )
  ) 
  
  ## Life Annuity ----

  output$life2PV <- renderText({
    if(input$life2Age + input$life2Duration > 109) stop("Maximum lifetime exceeded")
    formatC(
      axn(AM92Lt, input$life2Age, input$life2Duration, input$life2Interest/100, input$life2Defer, input$life2Pthly, payment = input$life2Type),
      digits = 4,
      format = "f"
    )
  })
  
  output$life2Rate <- renderText(
    formatC(
      (1 + input$life2Interest/100) ^ (input$life2Duration + input$life2Defer),
      digits = 6,
      format = "f"
    )
  )
  
  output$life2Factor <- renderText(
    formatC(
      discountRate(input$life2Interest/100) ^ (input$life2Duration + input$life2Defer),
      digits = 6,
      format = "f"
    )
  )
  
  output$life2Effective <- renderText(
    formatC(
      annualEffective(input$life2Interest/100),
      digits = 6,
      format = "f"
    )
  )
  
  output$life2Force <- renderText(
    formatC(
      forceOfInterest(input$life2Interest/100),
      digits = 6,
      format = "f"
    )
  ) 
  
  output$life2IntPthly <- renderText(
    formatC(
      interestPthly(input$life2Interest/100,input$life2Pthly),
      digits = 6,
      format = "f"
    )
  ) 
  
  output$life2EffectivePthly <- renderText(
    formatC(
      effectivePthly(input$life2Interest/100,input$life2Pthly),
      digits = 6,
      format = "f"
    )
  ) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
