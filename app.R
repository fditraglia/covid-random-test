#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Title goes here"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("sample_size",
                        "Sample Size:",
                        min = 500,
                        max = 5000,
                        value = 2500),
            
            sliderInput("prevalence",
                        "True Prevalance (%):",
                        min = 0.1,
                        max = 1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        n <- input$sample_size
        prevalence <- input$prevalence
        
        N <- 20000 # population size
        n_positive <- prevalence * N / 100 
        n_negative <- N - n_positive 
        
        
        lower <- qhyper(0.001, n_positive, n_negative, n) 
        upper <- qhyper(0.999, n_positive, n_negative, n) 
        detection_limit <- qhyper(0.2, n_positive, n_negative, n) 
        x <- lower:upper 
        detectable <- ifelse(x >= detection_limit, 'red', 'black')
  
        px <- dhyper(x, n_positive, n_negative, n)
        title1 <- paste0('True Prevalance: ', bquote(.(prevalence)), "%")
        title2 <- paste0('Sample Size: ', bquote(.(n)))
        plot(100 * x / n, px, type = 'h', ylab = 'Probability',  
             main = paste0(title1, ', ', title2), 
             xlab = 'Estimated Prevalance (%)',   col = detectable,  lwd = 2) 
        legend("topright", legend = c('20% Probability', '80% Probability'), text.col = c('black', 'red'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
