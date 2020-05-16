library(shiny)


# UI----
ui <- fluidPage(

    titlePanel("Normal Distribution"),

    sidebarLayout(
        sidebarPanel(
          numericInput("normMean",label=h3("Mean"), value = 0),
          numericInput("normVar",label=h3("Variance"), value = 1, min = 0),    
          numericInput("normX",label=h3("x"), value = 0)
        ),

        mainPanel(
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
        )
    )
)

# Functions----

normplot <- function(x, mean,variance){
  m <- mean
  sd <- sqrt(variance)
  
  dnormx <- function(x){
    return(dnorm(x,m,sd))
  }
  
  ggplot(NULL, aes(c(-4*sd+m,4*sd+m))) +
    geom_area(stat = "function", fun = dnormx, fill = "turquoise", xlim = c(-4*sd+m,4*sd+m))+
    geom_area(stat = "function", fun = dnormx, fill = "red", xlim = c(-4*sd+m,x))+
    geom_line(stat = "function", fun = dnormx, size = 0.6,  xlim = c(-4*sd+m,4*sd+m))+
    labs(x="",y="") 
}

# Output----
server <- function(input, output) {

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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
