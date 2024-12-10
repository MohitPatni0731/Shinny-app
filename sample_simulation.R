library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Statistical Power and Sample Size Calculator (Simulation)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode", "Mode:", 
                  choices = c("Calculate Sample Size" = "sample",
                              "Simulate Power" = "simulate")),
      
      numericInput("mean1", "Mean (Control):", value = 50, step = 0.1),
      numericInput("mean2", "Mean (Treatment):", value = 55, step = 0.1),
      numericInput("sd1", "Standard Deviation (Control):", value = 10, step = 0.1),
      numericInput("sd2", "Standard Deviation (Treatment):", value = 12, step = 0.1),
      numericInput("alpha", "Significance Level (\u03B1):", value = 0.05, step = 0.01),
      
      conditionalPanel(
        condition = "input.mode == 'simulate'",
        numericInput("n1", "Sample Size (Control):", value = 30, step = 1),
        numericInput("n2", "Sample Size (Treatment):", value = 30, step = 1),
        numericInput("simulations", "Number of Simulations:", value = 1000, step = 100)
      ),
      
      conditionalPanel(
        condition = "input.mode == 'sample'",
        numericInput("power", "Desired Power:", value = 0.8, step = 0.05),
        numericInput("simulations", "Number of Simulations:", value = 1000, step = 100)
      ),
      
      actionButton("calc", "Calculate")
    ),
    mainPanel(
      h3("Results"),
      verbatimTextOutput("results"),
      plotOutput("simulationPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Sample Size Calculation with Simulation
  calculate_sample_size <- reactive({
    input$calc
    isolate({
      target_power <- input$power
      simulations <- input$simulations
      n <- 10
      power_history <- numeric()
      
      while (TRUE) {
        reject_count <- sum(replicate(simulations, {
          control <- rnorm(n, mean = input$mean1, sd = input$sd1)
          treatment <- rnorm(n, mean = input$mean2, sd = input$sd2)
          t.test(control, treatment)$p.value <= input$alpha
        }))
        
        power <- reject_count / simulations
        power_history <- c(power_history, power)
        if (power >= target_power) break
        n <- n + 1
      }
      list(sample_size = n, achieved_power = power, power_history = power_history)
    })
  })
  
  # Power Simulation
  simulate_power <- reactive({
    input$calc
    isolate({
      n1 <- input$n1
      n2 <- input$n2
      simulations <- input$simulations
      reject_count <- sum(replicate(simulations, {
        control <- rnorm(n1, mean = input$mean1, sd = input$sd1)
        treatment <- rnorm(n2, mean = input$mean2, sd = input$sd2)
        t.test(control, treatment)$p.value <= input$alpha
      }))
      reject_count / simulations
    })
  })
  
  # Display Results
  output$results <- renderText({
    if (input$mode == "sample") {
      result <- calculate_sample_size()
      paste("Required Sample Size per Group:", result$sample_size, 
            "\nAchieved Power:", round(result$achieved_power, 3))
    } else if (input$mode == "simulate") {
      power <- simulate_power()
      paste("Estimated Statistical Power (Simulation):", round(power, 3))
    }
  })
  
  # Display Graph
  output$simulationPlot <- renderPlot({
    if (input$mode == "simulate") {
      power <- simulate_power()
      barplot(power, names.arg = "Power (Simulation)", 
              col = "lightblue", main = "Simulation-Based Power",
              ylim = c(0, 1))
    } else if (input$mode == "sample") {
      result <- calculate_sample_size()
      plot(seq(10, 10 + length(result$power_history) - 1),
           result$power_history, type = "o", pch = 19, col = "blue",
           xlab = "Sample Size", ylab = "Power",
           main = "Power vs. Sample Size")
      abline(h = input$power, col = "red", lty = 2)
      legend("bottomright", legend = c("Power Curve", "Target Power"),
             col = c("blue", "red"), lty = c(1, 2), pch = c(19, NA), bty = "n")
    }
  })
}

shinyApp(ui = ui, server = server)

