library(shiny)
library(actuar)

# UI----
ui <- fluidPage(

    titlePanel("Pareto Type-2 Distribution"),

    sidebarLayout(
        sidebarPanel(
          numericInput("pareto2X",label=h3("x"), value = 1, min = 0, step = 0.1),
          numericInput("pareto2A",label=h3(HTML(paste("&alpha;"))), value = 3, min = 0, step = 0.1),
          numericInput("pareto2K",label=h3(HTML(paste("&lambda;"))), value = 1, min = 0, step = 0.1)
        ),

        mainPanel(
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
        )
    )
)

# Functions----
pareto2plot <- function(x,a,k){
if(x <= 0 | a <= 0 | k <= 0) stop("Parameters must be greater than 0")

upper <- (9+k)/a

dpareto2x <- function(x){
  return(dpareto2(x,a,k))
}

ggplot(NULL, aes(c(0,upper))) +
  geom_area(stat = "function", fun = dpareto2x, fill = "turquoise", xlim = c(0,upper))+
  geom_area(stat = "function", fun = dpareto2x, fill = "red", xlim = c(0,x))+
  geom_line(stat = "function", fun = dpareto2x, size = 0.6, xlim = c(0,upper))+
  labs(x="",y="")
}

pareto2Mean <- function(a,k){
  if(a <= 1) stop("alpha must be greater than 1")
  
  k/(a-1)
}

pareto2Var <- function(a,k){
  if(a <= 2) stop("alpha must be greater than 2")
  
  (a*k^2)/((a-1)^2 * (a-2))
}


# Output----
server <- function(input, output) {

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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
