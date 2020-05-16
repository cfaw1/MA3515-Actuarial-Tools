library(shiny)
library(lifecontingencies)

# UI----
ui <- fluidPage(

    titlePanel("Increasing Annuity PV Calculator"),

    sidebarLayout(
        sidebarPanel(
          numericInput("interest2",label=h3("Interest rate (%)"), value = 4, min = 0),
          numericInput("duration2",label=h3("Duration (n)"), value = 5, min = 1),    
          radioButtons("type2", label=h3("Arrears or advance payments"),
                       choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears")
        ),


        mainPanel(
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
}

# Run the application 
shinyApp(ui = ui, server = server)
