ui <- fluidPage(
  titlePanel("Applicación ejercicio 1"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("numerouno", "#1", min = 10, max = 500, value = 100),
      textInput("numerodos", "#2", value = 3)
    ),
    mainPanel(
      textOutput("resultado1"),
      textOutput("resultado2"),
      textOutput("resultado3")
    )
  )
)

server <- function(input, output) {
  
  output$resultado1 <- renderText({
    x <- input$numerouno
    x
  })
  
  output$resultado2 <- renderText({
    input$numerodos
  })
  
  output$resultado3 <- renderText({
    input$numerouno + input$numerodos
  })
  
}