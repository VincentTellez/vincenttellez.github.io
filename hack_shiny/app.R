# Load required libraries
library(shiny)
library(ggplot2)
library(rsconnect)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Charts with mtcars Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chartType", "Choose Chart Type:", 
                  choices = c("Bar Chart", "Scatter Plot", "Bubble Chart")),
      conditionalPanel(
        condition = "input.chartType == 'Bar Chart'",
        selectInput("barVar", "Variable for Bar Chart:", 
                    choices = names(mtcars), selected = "cyl")
      ),
      conditionalPanel(
        condition = "input.chartType == 'Scatter Plot'",
        selectInput("xvar_scatter", "X-axis for Scatter Plot:", 
                    choices = names(mtcars), selected = "mpg"),
        selectInput("yvar_scatter", "Y-axis for Scatter Plot:", 
                    choices = names(mtcars), selected = "wt"),
        selectInput("color_scatter", "Color by:", 
                    choices = names(mtcars), selected = "cyl")
      ),
      conditionalPanel(
        condition = "input.chartType == 'Bubble Chart'",
        selectInput("xvar_bubble", "X-axis for Bubble Chart:", 
                    choices = names(mtcars), selected = "mpg"),
        selectInput("yvar_bubble", "Y-axis for Bubble Chart:", 
                    choices = names(mtcars), selected = "wt"),
        selectInput("size_bubble", "Bubble size:", 
                    choices = names(mtcars), selected = "hp"),
        selectInput("color_bubble", "Color by:", 
                    choices = names(mtcars), selected = "cyl")
      )
    ),
    mainPanel(
      plotOutput("chartOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$chartOutput <- renderPlot({
    # Bar Chart
    if (input$chartType == "Bar Chart") {
      ggplot(mtcars, aes_string(x = input$barVar)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Bar Chart of Selected Variable",
             x = input$barVar, 
             y = "Count") +
        theme_minimal()
      # Scatter Plot
    } else if (input$chartType == "Scatter Plot") {
      ggplot(mtcars, aes_string(x = input$xvar_scatter, y = input$yvar_scatter, color = input$color_scatter)) +
        geom_point(size = 3, alpha = 0.6) +
        labs(title = "Scatter Plot of Selected Variables",
             x = input$xvar_scatter, 
             y = input$yvar_scatter, 
             color = input$color_scatter) +
        theme_minimal()
      # Bubble Chart
    } else if (input$chartType == "Bubble Chart") {
      ggplot(mtcars, aes_string(x = input$xvar_bubble, y = input$yvar_bubble, 
                                size = input$size_bubble, color = input$color_bubble)) +
        geom_point(alpha = 0.6) +
        scale_size_continuous(range = c(3, 15)) +
        labs(title = "Bubble Chart of Selected Variables",
             x = input$xvar_bubble, 
             y = input$yvar_bubble, 
             size = input$size_bubble, 
             color = input$color_bubble) +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)