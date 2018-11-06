 
 
library(shiny)

 
ui <- fluidPage(
   
   
   titlePanel("Histogram and Scatterplot for Normal and Exponential Dataset "),
   
 
   sidebarLayout(
      sidebarPanel(
        radioButtons("dist", "Distribution type:",
                     c("Normal" = "norm",
                      "Exponential" = "exp")),
        br(),
        
         sliderInput("n",
                     "Number of bins:",
                     min = 1,
                     max = 5000,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                            tabPanel("Scatter Plot", plotOutput("plt")),
                            tabPanel("Histogram", plotOutput("Hist"))
         
      )
   
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   exp = rexp)
    
    dist(input$n)
  })
   
  output$plt <- renderPlot(

    plot((1:input$n), (d()), xlab = "x", ylab = "y", 
                                main = "Outleir Data Generation"  ))
  
  
  output$Hist <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

