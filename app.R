library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# Custom theme and styling
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#3498db",
  secondary = "#2ecc71"
)

# Define UI
ui <- fluidPage(
  theme = custom_theme,
  
  # Page title
  tags$head(
    tags$title("Distribution Explorer"),
    tags$style(HTML("
      .distribution-image-container {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 15px;
        margin-bottom: 20px;
      }
      .distribution-image {
        width: 150px;
        height: 120px;
        object-fit: cover;
        cursor: pointer;
        transition: transform 0.3s ease;
        border-radius: 10px;
        margin: 10px;
      }
      .distribution-image:hover {
        transform: scale(1.05);
        box-shadow: 0 4px 6px rgba(0,0,0,0.2);
      }
      .heavy-tailed-section {
        margin-top: 20px;
        text-align: center;
        padding: 15px;
        background-color: #f8f9fa;
        border-radius: 10px;
      }
    "))
  ),
  
  #titlePanel("Statistical Distribution Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Distribution Image Selection
      div(
        class = "distribution-image-container",
        
        # Add text above images
        div(
          style = "text-align: center; width: 100%; margin-bottom: 15px;",
          tags$h4("What is the shape of your data?")
        ),
        
        # First Row
        div(
          style = "display: flex; justify-content: center; width: 100%;",
          # Gamma Distribution Image
          tags$img(
            src = "gamma.jpg", 
            onclick = "Shiny.setInputValue('selected_distribution', 'gamma')",
            class = "distribution-image",
            alt = "Gamma Distribution"
          ),
          
          # Beta Distribution Image
          tags$img(
            src = "beta.jpg", 
            onclick = "Shiny.setInputValue('selected_distribution', 'beta')",
            class = "distribution-image",
            alt = "Beta Distribution"
          )
        ),
        
        # Second Row
        div(
          style = "display: flex; justify-content: center; width: 100%;",
          # Exponential Distribution Image
          tags$img(
            src = "exponential.jpg", 
            onclick = "Shiny.setInputValue('selected_distribution', 'exponential')",
            class = "distribution-image",
            alt = "Exponential Distribution"
          ),
          
          # Uniform Distribution Image
          tags$img(
            src = "uniform.jpg", 
            onclick = "Shiny.setInputValue('selected_distribution', 'uniform')",
            class = "distribution-image",
            alt = "Uniform Distribution"
          )
        )
      ),
      
      # Heavy-tailed Section
      div(
        class = "heavy-tailed-section",
        h4("Is your data heavy-tailed?"),
        radioButtons(
          inputId = "heavy_tailed", 
          label = NULL, 
          choices = c("Yes", "No"),
          selected = NULL  # No default selection
        )
      ),
      
      # Normal Distribution Parameters (only when heavy_tailed is 'No' and no distribution is selected)
      conditionalPanel(
        condition = "input.heavy_tailed == 'No' && (input.selected_distribution == null || input.selected_distribution == '')",
        numericInput("normal_mean", "Mean", value = 0, step = 0.1),
        numericInput("normal_sd", "Standard Deviation", value = 1, min = 0.1, step = 0.1)
      ),
      
      # Conditional Distribution Parameters
      uiOutput("distribution_params"),
      
      # Generate Data Button
      # Generate Data Button
div(
  style = "display: flex; justify-content: center; width: 100%; margin-top: 15px;",
  actionButton("generate_data", "Generate Data", class = "btn-primary")
)
    ),
    
    # Main Panel for Plotting
    mainPanel(
      plotlyOutput("distribution_plot", height = "500px")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive value to track if data generation button has been clicked
  data_generated <- reactiveVal(FALSE)
  
  # Store generated data
  generated_data <- reactiveVal(NULL)
  
  # Dynamic Distribution Parameters
  output$distribution_params <- renderUI({
    # Ensure distribution is selected and heavy_tailed is not 'No'
    if (!is.null(input$selected_distribution) && 
        input$selected_distribution != '' && 
        input$heavy_tailed != 'No') {
      switch(input$selected_distribution,
        "gamma" = tagList(
          numericInput("gamma_shape", "Shape", value = 2, min = 0.1, step = 0.1),
          numericInput("gamma_rate", "Rate", value = 1, min = 0.1, step = 0.1)
        ),
        "beta" = tagList(
          numericInput("beta_shape1", "Shape 1", value = 2, min = 0.1, step = 0.1),
          numericInput("beta_shape2", "Shape 2", value = 5, min = 0.1, step = 0.1)
        ),
        "exponential" = numericInput("exp_rate", "Rate", value = 1, min = 0.1, step = 0.1),
        "uniform" = tagList(
          numericInput("unif_min", "Minimum", value = 0, step = 0.1),
          numericInput("unif_max", "Maximum", value = 1, step = 0.1)
        )
      )
    }
  })
  
  # Data Generation when Generate Data button is clicked
  observeEvent(input$generate_data, {
    set.seed(123)
    n_samples <- 1000
    
    # Ensure heavy_tailed is not NULL
    heavy_tailed <- if (is.null(input$heavy_tailed)) NULL else input$heavy_tailed
    
    # Ensure selected_distribution is not NULL
    selected_distribution <- if (is.null(input$selected_distribution)) "" else input$selected_distribution
    
    # Data generation logic
    data <- if (selected_distribution == 'gamma') {
      shape <- if (is.null(input$gamma_shape)) 2 else input$gamma_shape
      rate <- if (is.null(input$gamma_rate)) 1 else input$gamma_rate
      rgamma(n_samples, shape = shape, rate = rate)
    } else if (selected_distribution == 'beta') {
      shape1 <- if (is.null(input$beta_shape1)) 2 else input$beta_shape1
      shape2 <- if (is.null(input$beta_shape2)) 5 else input$beta_shape2
      rbeta(n_samples, shape1 = shape1, shape2 = shape2)
    } else if (selected_distribution == 'exponential') {
      rate <- if (is.null(input$exp_rate)) 1 else input$exp_rate
      rexp(n_samples, rate = rate)
    } else if (selected_distribution == 'uniform') {
      min_val <- if (is.null(input$unif_min)) 0 else input$unif_min
      max_val <- if (is.null(input$unif_max)) 1 else input$unif_max
      runif(n_samples, min = min_val, max = max_val)
    } else if (heavy_tailed == 'No' && selected_distribution == '') {
      # Normal distribution with user-specified or default parameters
      mean <- if (is.null(input$normal_mean)) 0 else input$normal_mean
      sd <- if (is.null(input$normal_sd)) 1 else input$normal_sd
      rnorm(n_samples, mean = mean, sd = sd)
    } else {
      # Default to standard normal distribution
      rnorm(n_samples)
    }
    
    # Update generated data and flag
    generated_data(data)
    data_generated(TRUE)
  })
  
  # Distribution Plot
  output$distribution_plot <- renderPlotly({
    # Only plot if data has been generated
    req(data_generated())
    
    # Generate data frame from stored generated data
    data <- data.frame(value = generated_data())
    
    # Determine title based on selection
    heavy_tailed <- if (is.null(input$heavy_tailed)) NULL else input$heavy_tailed
    selected_distribution <- if (is.null(input$selected_distribution)) "" else input$selected_distribution
    
    plot_title <- if (selected_distribution != '') {
      paste(tools::toTitleCase(selected_distribution), "Distribution")
    } else if (heavy_tailed == 'No') {
      mean <- if (is.null(input$normal_mean)) 0 else input$normal_mean
      sd <- if (is.null(input$normal_sd)) 1 else input$normal_sd
      paste("Normal Distribution (Mean:", mean, ", SD:", sd, ")")
    } else {
      "Standard Normal Distribution"
    }
    
    p <- ggplot(data, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, 
                     fill = "#3498db", 
                     color = "white") +
      geom_density(color = "red", size = 1) +
      labs(
        title = plot_title,
        x = "Value",
        y = "Density"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui, server)
