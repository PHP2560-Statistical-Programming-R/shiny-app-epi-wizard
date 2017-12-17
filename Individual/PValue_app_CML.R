
library(shiny)
library(broman)

ui <- fluidPage(
  titlePanel(h1("P-Value Calculator for Normally Distributed Data")),
  br(), br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Zscore",
                  "Z-Score",
                  min=-6,
                  max= 6, 
                  value=0,
                  step= 0.1)
      ),
    mainPanel(
      textOutput("Pvalue")
      )
    )
  )




server <- function(input, output) {
  output$Pvalue <- renderText({ 
    paste("The p-value given the Z-Score is:",
          myround(pnorm(input$Zscore, mean = 0, sd=1, lower.tail = FALSE),3))
  })
}

shinyApp(ui = ui, server = server)

