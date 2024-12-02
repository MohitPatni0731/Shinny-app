# Load required libraries
library(shiny)
library(bslib)

# Define a custom image_button function as a clickable action button with an image and tooltip
image_button <- function(inputId, img_url, tooltip_text) {
  actionButton(
    inputId = inputId,
    label = tags$img(src = img_url, height = "100%", width = "100%"),
    class = "image-button",
    style = paste0(
      "width: 120px; height: 120px; border: none; background: none; padding: 0; cursor: pointer; "
    ),
    `data-toggle` = "tooltip",
    `data-placement` = "right",
    title = tooltip_text
  )
}

# Define UI
ui <- fluidPage(
  theme = bs_theme(),  # Use Bootstrap theme
  # Include Bootstrap's tooltip functionality
  tags$head(
    # Initialize Bootstrap tooltips
    tags$script(HTML("
      $(document).ready(function(){
        $('[data-toggle=\"tooltip\"]').tooltip(); 
      });
    ")),
    # Custom CSS for styling
    tags$style(HTML("
      /* Container Flexbox */
      .main-container {
        display: flex;
        flex-direction: row;
        height: 100vh;
        padding: 20px;
        box-sizing: border-box;
      }
      
      /* Left Panel */
      .left-panel {
        width: 30%;
        display: flex;
        flex-direction: column;
        align-items: center;
        padding-right: 20px;
        box-sizing: border-box;
      }
      
      /* Right Panel */
      .right-panel {
        width: 70%;
        padding-left: 10px; /* Reduced padding */
        box-sizing: border-box;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      
      /* Vertical Separator */
      .separator {
        border-left: 2px solid #ccc;
        height: 100%;
        margin: 0 20px;
      }
      
      /* Title Styling */
      .title {
        font-size: 1.5em;
        margin-bottom: 30px;
        text-align: center;
        font-weight: bold;
      }
      
      /* Image Button Hover Effect */
      .image-button:hover {
        transform: scale(1.05);
        transition: transform 0.2s;
      }
      
      /* Remove default button styling */
      .image-button:focus {
        outline: none;
        box-shadow: none;
      }
      
      /* Responsive Adjustments */
      @media (max-width: 768px) {
        .main-container {
          flex-direction: column;
        }
        
        .left-panel, .right-panel {
          width: 100%;
          padding: 10px;
        }
        
        .separator {
          display: none;
        }
      }
    "))
  ),
  
  # Main Container
  div(
    class = "main-container",
    
    # Left Panel
    div(
      class = "left-panel",
      
      # Title
      div(
        class = "title",
        "What is the shape of your data ?"
      ),
      
      # Icon Buttons using image_button
      image_button(
        inputId = "gamma_dist",
        img_url = "Gamma.jpg",  # Ensure this image exists in www/
        tooltip_text = "Gamma Distribution (Alpha)"
      ),
      
      br(),
      
      image_button(
        inputId = "beta_dist",
        img_url = "Beta.jpg",  # Ensure this image exists in www/
        tooltip_text = "Beta Distribution"
      ),
      
      br(),
      
      image_button(
        inputId = "exponential_dist",
        img_url = "Exponential.jpg",  # Ensure this image exists in www/
        tooltip_text = "Exponential Distribution"
      ),
      
      br(),
      
      image_button(
        inputId = "uniform_dist",
        img_url = "Uniform.jpg",  # Ensure this image exists in www/
        tooltip_text = "Uniform Distribution"
      )
    ),
    
    # Vertical Separator
    div(class = "separator"),
    
    # Right Panel
    div(
      class = "right-panel",
      
      # Output Plot with adjusted height and width
      plotOutput("selected_plot", height = "600px", width = "100%")
    )
  ),
  
  # Debugging Section: Display Image Paths
  hr(),
  h3("Debugging: Image Paths"),
  verbatimTextOutput("image_check")
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store selected distribution type
  selected_dist <- reactiveVal(NULL)
  
  # Observe event for each button
  observeEvent(input$gamma_dist, {
    selected_dist("gamma")
  })
  
  observeEvent(input$beta_dist, {
    selected_dist("beta")
  })
  
  observeEvent(input$exponential_dist, {
    selected_dist("exponential")
  })
  
  observeEvent(input$uniform_dist, {
    selected_dist("uniform")
  })
  
  # Render the selected plot
  output$selected_plot <- renderPlot({
    dist_type <- selected_dist()
    
    if (is.null(dist_type)) {
      # Default message when no distribution is selected
      plot.new()
      text(0.5, 0.5, "Please select a distribution from the left.", cex = 1.5)
    } else {
      set.seed(123)  # For reproducibility
      # Adjust margins to be smaller
      par(mar = c(4, 4, 2, 1))  # bottom, left, top, right
      switch(dist_type,
             "gamma" = {
               # Gamma distribution with shape = 2, rate = 1
               data <- rgamma(1000, shape = 2, rate = 1)
               hist(data, breaks = 30, main = "Gamma Distribution (Alpha)", 
                    xlab = "Value", col = "#1f77b4", border = "white")
             },
             "beta" = {
               # Beta distribution with shape1 = 2, shape2 = 5
               data <- rbeta(1000, shape1 = 2, shape2 = 5)
               hist(data, breaks = 30, main = "Beta Distribution", 
                    xlab = "Value", col = "#ff7f0e", border = "white", probability = TRUE)
               curve(dbeta(x, shape1 = 2, shape2 = 5), add = TRUE, col = "black", lwd = 2)
             },
             "exponential" = {
               # Exponential distribution with rate = 1
               data <- rexp(1000, rate = 1)
               hist(data, breaks = 30, main = "Exponential Distribution", 
                    xlab = "Value", col = "#2ca02c", border = "white")
             },
             "uniform" = {
               # Uniform distribution between 0 and 1
               data <- runif(1000, min = 0, max = 1)
               hist(data, breaks = 30, main = "Uniform Distribution", 
                    xlab = "Value", col = "#d62728", border = "white")
             }
      )
    }
  })
  
  # Debugging: List Files in www Directory
  output$image_check <- renderPrint({
    # List files in www directory
    www_path <- file.path(getwd(), "www")
    if (dir.exists(www_path)) {
      list_files = list.files(www_path)
      list_files
    } else {
      "www directory does not exist."
    }
  })
}

# Run the Shiny App
shinyApp(ui, server)
