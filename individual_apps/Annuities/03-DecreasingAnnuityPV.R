library(shiny)
library(lifecontingencies)

# UI----
ui <- fluidPage(

    titlePanel("Decreasing Annuity PV Calculator"),

    sidebarLayout(
        sidebarPanel(
            numericInput("interest3",label=h3("Interest rate (%)"), value = 4, min = 0),
            numericInput("duration3",label=h3("Duration (n)"), value = 5, min = 1),    
            radioButtons("type3", label=h3("Arrears or advance payments"),
                         choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears")
        ),

        mainPanel(
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
        )
    )
)

# Functions----

discountRate <- function(i){
  1 / (1 + i)
}

annualEffective <- function(i){
  i / (i + 1)
}

forceOfInterest <- function(i){
  -log(1 - annualEffective(i))
}

interestPthly <- function(i,p=1){
  p * ((1+i)^(1/p) - 1)
}

effectivePthly <- function(i,p=1){
  p * (1-(1-annualEffective(i))^(1/p))
}

# Output----
server <- function(input, output) {

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
      discountRate(input$interest3/100) ^ input$duration3,
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
}

# Run the application 
shinyApp(ui = ui, server = server)
