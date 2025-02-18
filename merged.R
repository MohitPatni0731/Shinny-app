# Integrated Apollo Dashboard Code
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(future)
library(promises)
plan(multisession)  # Enable parallel processing

# ---------------------------- #
#       Shared Components      #
# ---------------------------- #
apollo_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2c3e50",
  secondary = "#3498db",
  success = "#18bc9c"
)

shared_data <- reactiveVal(NULL)

# ---------------------------- #
#     Mohit's Distribution     #
#        Explorer Module       # 
# ---------------------------- #
mohit_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        div(class = "distribution-image-container",
            div(style = "text-align: center; width: 100%; margin-bottom: 15px;",
                tags$h4("What is the shape of your data?")),
            div(style = "display: flex; justify-content: center; width: 100%;",
                tags$img(src = "gamma.jpg", onclick = paste0("Shiny.setInputValue('", ns("selected_distribution"), "', 'gamma')"), class = "distribution-image"),
                tags$img(src = "beta.jpg", onclick = paste0("Shiny.setInputValue('", ns("selected_distribution"), "', 'beta')"), class = "distribution-image")),
            div(style = "display: flex; justify-content: center; width: 100%;",
                tags$img(src = "exponential.jpg", onclick = paste0("Shiny.setInputValue('", ns("selected_distribution"), "', 'exponential')"), class = "distribution-image"),
                tags$img(src = "uniform.jpg", onclick = paste0("Shiny.setInputValue('", ns("selected_distribution"), "', 'uniform')"), class = "distribution-image"))
        ),
        div(class = "heavy-tailed-section",
            h4("Is your data heavy-tailed?"),
            radioButtons(ns("heavy_tailed"), NULL, c("Yes", "No"))),
        uiOutput(ns("distribution_params")),
        actionButton(ns("generate_data"), "Generate Data", class = "btn-primary"),
        div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 10px;",
            h4("Statistical Tests"),
            radioButtons(ns("t_test_type"), "T-Test Type:", c("One Sample", "Two Sample")),
            conditionalPanel(condition = paste0("input['", ns("t_test_type"), "'] == 'One Sample'"),
                             numericInput(ns("mu"), "Hypothesized Mean:", 0),
                             numericInput(ns("conf_level"), "Confidence Level:", 0.95, 0, 1, 0.01)),
            conditionalPanel(condition = paste0("input['", ns("t_test_type"), "'] == 'Two Sample'"),
                             numericInput(ns("sample2_size"), "Sample 2 Size:", 1000),
                             numericInput(ns("sample2_mean"), "Sample 2 Mean:", 0),
                             numericInput(ns("sample2_sd"), "Sample 2 SD:", 1, 0.1),
                             numericInput(ns("conf_level_2"), "Confidence Level:", 0.95, 0, 1, 0.01)),
            actionButton(ns("run_ttest"), "Run T-Test", class = "btn-primary"))
      ),
      mainPanel(
        plotlyOutput(ns("distribution_plot"), height = "500px"),
        conditionalPanel(condition = paste0("input['", ns("run_ttest"), "'] > 0"),
                         div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 10px;",
                             h4("T-Test Results"),
                             verbatimTextOutput(ns("ttest_results"))))
      )
    )
  )
}

mohit_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_generated <- reactiveVal(FALSE)
    generated_data <- reactiveVal(NULL)
    
    output$distribution_params <- renderUI({
      if (!is.null(input$selected_distribution) && input$heavy_tailed != 'No') {
        switch(input$selected_distribution,
               "gamma" = tagList(
                 numericInput(session$ns("gamma_shape"), "Shape", 2, 0.1),
                 numericInput(session$ns("gamma_rate"), "Rate", 1, 0.1)),
               "beta" = tagList(
                 numericInput(session$ns("beta_shape1"), "Shape 1", 2, 0.1),
                 numericInput(session$ns("beta_shape2"), "Shape 2", 5, 0.1)),
               "exponential" = numericInput(session$ns("exp_rate"), "Rate", 1, 0.1),
               "uniform" = tagList(
                 numericInput(session$ns("unif_min"), "Minimum", 0),
                 numericInput(session$ns("unif_max"), "Maximum", 1)))
      }
    })
    
    observeEvent(input$generate_data, {
      set.seed(123)
      n <- 1000
      data <- switch(input$selected_distribution,
                     "gamma" = rgamma(n, input$gamma_shape, input$gamma_rate),
                     "beta" = rbeta(n, input$beta_shape1, input$beta_shape2),
                     "exponential" = rexp(n, input$exp_rate),
                     "uniform" = runif(n, input$unif_min, input$unif_max),
                     rnorm(n, input$normal_mean, input$normal_sd))
      generated_data(data)
      shared_data(data)  # Share data with Alex's module
      data_generated(TRUE)
    })
    
    output$distribution_plot <- renderPlotly({
      req(data_generated())
      data <- data.frame(value = generated_data())
      p <- ggplot(data, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3498db") +
        geom_density(color = "red", size = 1) +
        labs(title = "Generated Distribution", x = "Value", y = "Density")
      ggplotly(p)
    })
    
    output$ttest_results <- renderPrint({
      req(input$run_ttest, generated_data())
      sample1 <- generated_data()
      
      if (input$t_test_type == "One Sample") {
        test <- t.test(sample1, mu = input$mu, conf.level = input$conf_level)
        cat("One-Sample T-Test:\n",
            "t(", round(test$parameter, 2), ") = ", round(test$statistic, 4), 
            ", p = ", format.pval(test$p.value, digits = 4), "\n",
            "95% CI [", round(test$conf.int[1], 4), ", ", 
            round(test$conf.int[2], 4), "]", sep = "")
      } else {
        sample2 <- rnorm(input$sample2_size, input$sample2_mean, input$sample2_sd)
        test <- t.test(sample1, sample2, conf.level = input$conf_level_2)
        cat("Two-Sample T-Test:\n",
            "t(", round(test$parameter, 2), ") = ", round(test$statistic, 4), 
            ", p = ", format.pval(test$p.value, digits = 4), "\n",
            "Mean Difference: ", round(diff(test$estimate), 4), "\n",
            "95% CI [", round(test$conf.int[1], 4), ", ", 
            round(test$conf.int[2], 4), "]", sep = "")
      }
    })
  })
}

# ---------------------------- #
#   Alex's Power Analysis      #
#         Module               #
# ---------------------------- #
alex_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("mode"), "Mode:", c("Sample Size Calculation", "Power Simulation")),
        numericInput(ns("mean1"), "Control Mean:", 50, 0.1),
        numericInput(ns("mean2"), "Treatment Mean:", 55, 0.1),
        numericInput(ns("sd1"), "Control SD:", 10, 0.1),
        numericInput(ns("sd2"), "Treatment SD:", 12, 0.1),
        numericInput(ns("alpha"), "Alpha:", 0.05, 0.01, 0.99, 0.01),
        
        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'Power Simulation'"),
          numericInput(ns("n1"), "Control N:", 30, 1),
          numericInput(ns("n2"), "Treatment N:", 30, 1)),
        
        conditionalPanel(
          condition = paste0("input['", ns("mode"), "'] == 'Sample Size Calculation'"),
          numericInput(ns("power"), "Target Power:", 0.8, 0.05, 0.99, 0.05)),
        
        numericInput(ns("simulations"), "Simulations:", 1000, 100, 10000, 100),
        actionButton(ns("calc"), "Run Analysis", class = "btn-primary")
      ),
      mainPanel(
        h3("Analysis Results"),
        verbatimTextOutput(ns("results")),
        plotOutput(ns("power_plot"))
      )
    )
  )
}

alex_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    power_results <- reactiveVal(NULL)
    
    observeEvent(input$calc, {
      if (input$mode == "Sample Size Calculation") {
        req(input$power)
        future({
          n <- 10
          power_hist <- numeric()
          while(TRUE) {
            rejects <- sum(replicate(input$simulations, {
              control <- rnorm(n, input$mean1, input$sd1)
              treatment <- rnorm(n, input$mean2, input$sd2)
              t.test(control, treatment)$p.value <= input$alpha
            }))
            current_power <- rejects/input$simulations
            power_hist <- c(power_hist, current_power)
            if(current_power >= input$power) break
            n <- n + 1
          }
          list(n = n, power_hist = power_hist)
        }) %...>% power_results()
      } else {
        req(input$n1, input$n2)
        future({
          rejects <- sum(replicate(input$simulations, {
            control <- rnorm(input$n1, input$mean1, input$sd1)
            treatment <- rnorm(input$n2, input$mean2, input$sd2) 
            t.test(control, treatment)$p.value <= input$alpha
          }))
          rejects/input$simulations
        }) %...>% power_results()
      }
    })
    
    output$results <- renderText({
      req(power_results())
      if (input$mode == "Sample Size Calculation") {
        paste("Required sample size per group:", power_results()$n,
              "\nAchieved power:", round(tail(power_results()$power_hist,1), 3))
      } else {
        paste("Estimated power:", round(power_results(), 3))
      }
    })
    
    output$power_plot <- renderPlot({
      req(power_results())
      if (input$mode == "Sample Size Calculation") {
        plot(10:(10+length(power_results()$power_hist)-1), power_results()$power_hist,
             type = 'b', xlab = "Sample Size", ylab = "Power",
             main = "Power Curve")
        abline(h = input$power, col = "red", lty = 2)
      } else {
        barplot(power_results(), ylim = c(0,1), col = "#3498db",
                main = "Power Simulation Results")
      }
    })
  })
}

# ---------------------------- #
#       Main Application       #
# ---------------------------- #
ui <- navbarPage(
  title = "Apollo Statistical Suite",
  theme = apollo_theme,
  tabPanel("Distribution Explorer", mohit_UI("mohit")),
  tabPanel("Power Analysis", alex_UI("alex")),
  footer = div(style = "text-align: center; padding: 20px;",
               "Apollo Dashboard v1.0 | Integrated Statistical Analysis Platform")
)

server <- function(input, output, session) {
  mohit_Server("mohit")
  alex_Server("alex")
  
  # Cross-module communication
  observe({
    if(!is.null(shared_data())) {
      updateNumericInput(session, "alex-mean1", value = round(mean(shared_data()), 2))
      updateNumericInput(session, "alex-sd1", value = round(sd(shared_data()), 2))
    }
  })
}

shinyApp(ui, server)
