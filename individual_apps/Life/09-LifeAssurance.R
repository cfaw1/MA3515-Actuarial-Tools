library(shiny)
library(lifecontingencies)

# UI----
ui <- fluidPage(

    titlePanel("Life Assurance PV"),

    sidebarLayout(
        sidebarPanel(
          numericInput("lifeInterest",label=h3("Interest rate (%)"), value = 4, min = 0),
          numericInput("lifeAge",label=h3("Age (years)"), value = 40, min = 0, max = 109),
          numericInput("lifeDuration",label=h3("Duration (years)"), value = 5, min = 1),    
          numericInput("lifeDefer",label=h3("Deferral period (years)"), value = 0, min = 0)
        ),


        mainPanel(
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
