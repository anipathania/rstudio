library(shiny)

# Load data (replace with your actual data if needed)
data <- structure(list(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 5, 4, 5)
), .Names = c("x", "y"))

# Define UI elements
ui <- fluidPage(
  titlePanel("Simple Linear Regression"),
  sidebarLayout(
    sidebarPanel(
      # Input for selecting independent variable
      selectInput("x_var", "Choose X variable:", names(data)),
      # Input for selecting dependent variable
      selectInput("y_var", "Choose Y variable:", names(data)),
      # Button to trigger regression analysis
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      # Placeholder for displaying results
      verbatimTextOutput("output")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to run regression based on user selections
  regression_result <- reactive({
    if (is.null(input$x_var) | is.null(input$y_var)) {
      return(NULL)
    } else {
      lm(formula = paste(input$y_var, "~", input$x_var), data = data)
    }
  })
  
  # Display regression summary in output
  output$output <- renderPrint({
    summary(regression_result())
  })
}

# Generate plot based on regression model
output$plot <- renderPlot({
  if (is.null(regression_result())) {
    return()
  }
  
  ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = lm, regression_result()) +
    labs(title = paste("Regression of", input$y_var, "on", input$x_var),
         x = input$x_var, y = input$y_var)
})
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
