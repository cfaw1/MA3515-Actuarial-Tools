library(shiny)

# UI----
ui <- fluidPage(

    titlePanel("Binomial Distribution"),

    sidebarLayout(
        sidebarPanel(
          numericInput("binN",label=h3("n"), value = 10, min = 0),
          numericInput("binP",label=h3("p"), value = 0.5, min = 0, max = 1, step = 0.05),    
          numericInput("binX",label=h3("x"), value = 5, min=0)
        ),

        mainPanel(
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
    )
)

# Functions----

binplot <- function(x,n,p){
  data <- dbinom(x=0:n,n,p)
  names(data) <- 0:n
  columns <- rep("turquoise", n + 1)
  columns[x+1] <- "red"
  columns[0:x] <- "indianred1"
  barplot(data, col = columns)
}

# Output----
server <- function(input, output) {

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
}

# Run the application 
shinyApp(ui = ui, server = server)
