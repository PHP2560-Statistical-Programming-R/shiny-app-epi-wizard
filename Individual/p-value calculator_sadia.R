library(shiny)

ui <- fluidPage(
  tags$head(tags$style("#p{color: red;
                        text-align:center;
                        font-size: 20px;
                         font-style: bold;
                       }"
                         )
  ),
  titlePanel("Show p" ),
  sidebarLayout(
    sidebarPanel(
      numericInput("z", label = "enter Z score here", value = 0),
      radioButtons("t", "Test", c("one-sided", "two-sided"))),
      #textInput("tail", label = "enter one tail or two tail")),
      mainPanel(textOutput("p"))
   )
)

server <- function(input, output) {
  output$p <- renderText({
    z <- input$z
    # p value for a 2 tailed test
    if (input$t == "two-sided"){
    p.value <- 2*pnorm(abs(z),lower.tail=FALSE)}
    # p value for a 1 tailed test
    else {p.value <- pnorm(abs(z),lower.tail=FALSE)}
    paste("p value for", input$t, "test is:", round(p.value,5))
    
  })
  
}

shinyApp(ui, server)
