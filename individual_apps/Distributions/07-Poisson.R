library(shiny)

# UI----
ui <- fluidPage(

    titlePanel("Poisson Distribution"),

    sidebarLayout(
        sidebarPanel(
          numericInput("poX",label=h3("x"), value = 1, min = 1),
          numericInput("poK",label=h3(HTML(paste("&lambda;"))), value = 1, min = 1)
        ),

        mainPanel(
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
    )
)

# Functions----

poplot <- function(x,k){
  n = 8 + (k-1)*2
  data <- dpois(x=0:n,k)
  names(data) <- 0:n
  columns <- rep("turquoise", n + 1)
  columns[x+1] <- "red"
  columns[0:x] <- "indianred1"
  barplot(data, col = columns)
}

# Output----
server <- function(input, output) {

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
}

# Run the application 
shinyApp(ui = ui, server = server)
