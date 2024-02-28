library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Choose independent variable:", choices = colnames(mtcars)),
      selectInput("y_var", "Choose dependent variable:", choices = colnames(mtcars))
    ),
    mainPanel(
      plotOutput("scatterplot"),
      verbatimTextOutput("regression_summary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = input$x_var, y = input$y_var) +
      theme_minimal()
  })
  
  # Perform regression analysis and display summary
  output$regression_summary <- renderPrint({
    lm_model <- lm(paste(input$y_var, "~", input$x_var), data = mtcars)
    summary(lm_model)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

