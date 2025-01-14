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
          selected = NULL
        )
      ),
      
      # Normal Distribution Parameters
      conditionalPanel(
        condition = "input.heavy_tailed == 'No' && (input.selected_distribution == null || input.selected_distribution == '')",
        numericInput("normal_mean", "Mean", value = 0, step = 0.1),
        numericInput("normal_sd", "Standard Deviation", value = 1, min = 0.1, step = 0.1)
      ),
      
      # Conditional Distribution Parameters
      uiOutput("distribution_params"),
      
      # Generate Data Button
      div(
        style = "display: flex; justify-content: center; width: 100%; margin-top: 15px;",
        actionButton("generate_data", "Generate Data", class = "btn-primary")
      ),
      
      # T-Test Section
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 10px;",
        h4("Statistical Tests"),
        
        # T-test type selection
        radioButtons(
          "t_test_type",
          "Select T-Test Type:",
          choices = c(
            "One Sample" = "one_sample",
            "Two Sample" = "two_sample"
          )
        ),
        
        # Conditional panels for different t-test types
        conditionalPanel(
          condition = "input.t_test_type == 'one_sample'",
          numericInput("mu", "Hypothesized Mean (μ₀):", value = 0),
          numericInput("conf_level", "Confidence Level:", value = 0.95, min = 0, max = 1, step = 0.01)
        ),
        
        conditionalPanel(
          condition = "input.t_test_type == 'two_sample'",
          numericInput("sample2_size", "Sample 2 Size:", value = 1000, min = 1),
          numericInput("sample2_mean", "Sample 2 Mean:", value = 0),
          numericInput("sample2_sd", "Sample 2 SD:", value = 1, min = 0.1),
          numericInput("conf_level_2", "Confidence Level:", value = 0.95, min = 0, max = 1, step = 0.01)
        ),
        
        actionButton("run_ttest", "Run T-Test", class = "btn-primary")
      )
    ),
    
    mainPanel(
      plotlyOutput("distribution_plot", height = "500px"),
      
      # T-Test Results Section
      conditionalPanel(
        condition = "input.run_ttest > 0",
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 10px;",
          h4("T-Test Results"),
          verbatimTextOutput("ttest_results")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive values
  data_generated <- reactiveVal(FALSE)
  generated_data <- reactiveVal(NULL)
  
  # Dynamic Distribution Parameters
  output$distribution_params <- renderUI({
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
  
  # Data Generation
  observeEvent(input$generate_data, {
    set.seed(123)
    n_samples <- 1000
    
    heavy_tailed <- if (is.null(input$heavy_tailed)) NULL else input$heavy_tailed
    selected_distribution <- if (is.null(input$selected_distribution)) "" else input$selected_distribution
    
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
      mean <- if (is.null(input$normal_mean)) 0 else input$normal_mean
      sd <- if (is.null(input$normal_sd)) 1 else input$normal_sd
      rnorm(n_samples, mean = mean, sd = sd)
    } else {
      rnorm(n_samples)
    }
    
    generated_data(data)
    data_generated(TRUE)
  })
  
  # Distribution Plot
  output$distribution_plot <- renderPlotly({
    req(data_generated())
    
    data <- data.frame(value = generated_data())
    
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
  
  # T-Test Results
  output$ttest_results <- renderPrint({
    req(input$run_ttest)
    req(data_generated())
    
    sample1_data <- generated_data()
    
    validate(
      need(!is.null(sample1_data), "Please generate data first by clicking the 'Generate Data' button.")
    )
    
    if (input$t_test_type == "one_sample") {
      # Perform one-sample t-test
      test_result <- t.test(sample1_data, 
                          mu = input$mu, 
                          conf.level = input$conf_level)
      
      cat("One-Sample T-Test Results:\n\n")
      cat("Hypothesized mean (μ₀):", input$mu, "\n")
      cat("Sample mean:", round(mean(sample1_data), 4), "\n")
      cat("Sample SD:", round(sd(sample1_data), 4), "\n")
      cat("t-statistic:", round(test_result$statistic, 4), "\n")
      cat("degrees of freedom:", round(test_result$parameter, 2), "\n")
      cat("p-value:", format.pval(test_result$p.value, digits = 4), "\n")
      cat(paste0(input$conf_level * 100, "% Confidence Interval:\n"))
      cat("[", round(test_result$conf.int[1], 4), ", ", 
          round(test_result$conf.int[2], 4), "]\n")
      
    } else if (input$t_test_type == "two_sample") {
      # Generate second sample based on user inputs
      set.seed(124)  # Different seed from first sample
      sample2_data <- rnorm(input$sample2_size, 
                          mean = input$sample2_mean, 
                          sd = input$sample2_sd)
      
      # Perform two-sample t-test
      test_result <- t.test(sample1_data, sample2_data, 
                          conf.level = input$conf_level_2)
      
      cat("Two-Sample T-Test Results:\n\n")
      cat("Sample 1:\n")
      cat("  Mean:", round(mean(sample1_data), 4), "\n")
      cat("  SD:", round(sd(sample1_data), 4), "\n")
      cat("  Size:", length(sample1_data), "\n\n")
      
      cat("Sample 2:\n")
      cat("  Mean:", round(mean(sample2_data), 4), "\n")
      cat("  SD:", round(sd(sample2_data), 4), "\n")
      cat("  Size:", length(sample2_data), "\n\n")
      
      cat("Test Statistics:\n")
      cat("t-statistic:", round(test_result$statistic, 4), "\n")
      cat("degrees of freedom:", round(test_result$parameter, 2), "\n")
      cat("p-value:", format.pval(test_result$p.value, digits = 4), "\n")
      cat(paste0(input$conf_level_2 * 100, "% Confidence Interval for difference in means:\n"))
      cat("[", round(test_result$conf.int[1], 4), ", ", 
          round(test_result$conf.int[2], 4), "]\n")
    }
  })
}

# Run the application 
shinyApp(ui, server)
