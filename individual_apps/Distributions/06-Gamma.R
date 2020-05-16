library(shiny)

# UI----
ui <- fluidPage(

    titlePanel("Gamma Distribution"),

    sidebarLayout(
        sidebarPanel(
          numericInput("gammaA",label=h3(HTML(paste("&alpha;"))), value = 1, min = 0, step = 0.5),
          numericInput("gammaB",label=h3(HTML(paste("&beta;"))), value = 1, min = 0, step = 0.5),    
          numericInput("gammaX",label=h3("x"), value = 1, min=0, step = 0.5)
        ),

        mainPanel(
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
        ) #main
    )
)

# Functions----

gammaplot <- function(x,a,b){
  if(x<=0 |a <=0 | b <=0) stop("Parameters must be greater than 0")
  upper <- 3*(a-1)+(9/b)
  
  dgammax <- function(x){
    return(dgamma(x,a,b))
  }
  
  ggplot(NULL, aes(c(0,upper))) +
    geom_area(stat = "function", fun = dgammax, fill = "turquoise", xlim = c(0,upper))+
    geom_area(stat = "function", fun = dgammax, fill = "red", xlim = c(0,x))+
    geom_line(stat = "function", fun = dgammax, size = 0.6, xlim = c(0,upper))+
    labs(x="",y="")
}

# Output----
server <- function(input, output) {

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
}

# Run the application 
shinyApp(ui = ui, server = server)
