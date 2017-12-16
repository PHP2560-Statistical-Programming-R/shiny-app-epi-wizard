library(shiny)

ui <- fluidPage(
  h1("the probability of achieving z value in a standard normal distribution"),
  wellPanel(sliderInput(inputId = "num", 
              label = "Z value", 
              value = 1, step = 0.01, min = 0, max = 2)),
  textOutput("pvalue")
)

server <- function(input, output) {
  output$pvalue <- renderText({ cbind("p-value",
    round(pnorm(input$num, mean = 0, sd=1, lower.tail = FALSE),3))
  })
}

shinyApp(ui = ui, server = server)