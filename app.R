library(shiny)
library(bslib)

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

ui <- fluidPage(
  theme = bs_theme(), 
  tags$head(
    tags$script(HTML("
      $(document).ready(function(){
        $('[data-toggle=\"tooltip\"]').tooltip(); 
      });
    ")),
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
  
  div(
    class = "main-container",
    
    div(
      class = "left-panel",
      
      radioButtons("own_data", "Do you have your own data?", choices = c("Yes", "No"), selected = "No"),
      
      conditionalPanel(
        condition = "input.own_data == 'Yes'",
        
        div(
          class = "title",
          "What is the shape of your data?"
        ),
        
        image_button(
          inputId = "gamma_dist",
          img_url = "Gamma.jpg",  
          tooltip_text = "Gamma Distribution (Alpha)"
        ),
        
        br(),
        
        image_button(
          inputId = "beta_dist",
          img_url = "Beta.jpg",  
          tooltip_text = "Beta Distribution"
        ),
        
        br(),
        
        image_button(
          inputId = "exponential_dist",
          img_url = "Exponential.jpg",  
          tooltip_text = "Exponential Distribution"
        ),
        
        br(),
        
        image_button(
          inputId = "uniform_dist",
          img_url = "Uniform.jpg",  
          tooltip_text = "Uniform Distribution"
        )
      ),
      
      conditionalPanel(
         condition = "input.own_data == 'Yes'",
         radioButtons("heavy_tailed", "Is your data heavy-tailed?", choices = c("Yes", "No"), selected = NULL)
       ),
       
       conditionalPanel(
         condition = "input.heavy_tailed == 'No'",
         numericInput("mean", "Mean:", value = 0),
         numericInput("sd", "Standard Deviation:", value = 1)
       )
    ),
    
    div(class = "separator"),
    
    div(
      class = "right-panel",
      
      plotOutput("selected_plot", height = "600px", width = "100%")
    )
  ),
  
  hr(),
  h3("Debugging: Image Paths"),
  verbatimTextOutput("image_check")
)

server <- function(input, output, session) {
  
  selected_dist <- reactiveVal(NULL)
  
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
  
   observeEvent(input$heavy_tailed, {
     if (input$heavy_tailed == 'No') {
       selected_dist('normal')
     } else {
       selected_dist(NULL)
     }
   })

   output$selected_plot <- renderPlot({
     dist_type <- selected_dist()
     
     if (is.null(dist_type)) {
       plot.new()
       text(0.5,0.5,"Please select a distribution from the left.",cex=1.5)
     } else {
       set.seed(123) 
       par(mar=c(4,4,2,1))
       switch(dist_type,
              'gamma'={
                data<-rgamma(1000,shape=2,rate=1)
                hist(data,breaks=30,main="Gamma Distribution (Alpha)",
                     xlab="Value",col="#1f77b4",border="white")
              },
              'beta'={
                data<-rbeta(1000,shape1=2,shape2=5)
                hist(data,breaks=30,main="Beta Distribution",
                     xlab="Value",col="#ff7f0e",border="white",probability=TRUE)
                curve(dbeta(x,shape1=2,shape2=5),add=TRUE,col="black",lwd=2)
              },
              'exponential'={
                data<-rexp(1000,rate=1)
                hist(data,breaks=30,main="Exponential Distribution",
                     xlab="Value",col="#2ca02c",border="white")
              },
              'uniform'={
                data<-runif(1000,min=0,max=1)
                hist(data,breaks=30,main="Uniform Distribution",
                     xlab="Value",col="#d62728",border="white")
              },
              'normal'={
                mean_val <- input$mean
                sd_val <- input$sd
                
                if (!is.null(mean_val) && !is.null(sd_val)) {
                  data <- rnorm(1000, mean=mean_val, sd=sd_val)
                  hist(data, breaks=30, main=paste("Normal Distribution\nMean:", mean_val,
                                                   "\nSD:", sd_val),
                       xlab="Value", col="#9467bd", border="white")
                } else {
                  plot.new()
                  text(0.5,0.5,"Please enter valid Mean and SD.",cex=1.5)
                }
              }
       )
     }
   })
   
   output$image_check <- renderPrint({
     www_path <- file.path(getwd(), 'www')
     if (dir.exists(www_path)) {
       list.files(www_path)
     } else {
       'www directory does not exist.'
     }
   })
}

shinyApp(ui=ui, server=server)
