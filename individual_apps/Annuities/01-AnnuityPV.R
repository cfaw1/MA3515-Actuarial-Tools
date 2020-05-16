library(shiny)
library(lifecontingencies)

# UI----
ui <- fluidPage(

    titlePanel("Annuity Present Value Calculator"),

    sidebarLayout(
        sidebarPanel(
          numericInput("interest",label=h3("Interest rate (%)"), value = 4, min = 0),
          numericInput("duration",label=h3("Duration (years)"), value = 5, min = 0),    
          numericInput("defer",label=h3("Deferral period (years)"), value = 0, min = 0),
          numericInput("pthly",label=h3("Payments per year"), value = 1, min = 1), 
          radioButtons("type", label=h3("Arrears or advance payments"),
                       choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears")
        ),


        mainPanel(
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
}

# Run the application 
shinyApp(ui = ui, server = server)
