library(shiny)
library(lifecontingencies)

# UI----
ui <- fluidPage(

    titlePanel("Life Assurance PV"),

    sidebarLayout(
        sidebarPanel(
          numericInput("life2Interest",label=h3("Interest rate (%)"), value = 4, min = 0),
          numericInput("life2Age",label=h3("Age (years)"), value = 40, min = 0, max = 109),
          numericInput("life2Duration",label=h3("Duration (years)"), value = 5, min = 1),    
          numericInput("life2Defer",label=h3("Deferral period (years)"), value = 0, min = 0),
          numericInput("life2Pthly",label=h3("Payments per year"), value = 1, min = 1), 
          radioButtons("life2Type", label=h3("Arrears or advance payments"),
                       choices = list("Arrears" = "arrears", "Advance" = "advance"), selected = "arrears")
        ),


        mainPanel(
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
