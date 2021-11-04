#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Where is the Mean?"),

    # Sidebar with a reset button
    sidebarLayout(
        sidebarPanel(
           fluidRow(
               sliderInput(inputId = "sampleSize",
                           label = "Select Sample Size",
                           min = 2,
                           max = 30,
                           value = 16
               )
           ),
           fluidRow(
               headerPanel("")
           ),
           fluidRow(
               actionButton("checkButton", "Check Answer", width = 120),
               actionButton("retryButton", "Try Again", width = 120)
           ),
           fluidRow(
               headerPanel("")
           ),
           fluidRow(
               textOutput("statusMessage")
           )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("dotPlot",
                      click = clickOpts(id = "dotClick")
            )
        )
    )
)

# Define server logic required to draw the dot plot
server <- function(input, output) {

    plotData <- reactiveValues(data = NULL)
    meanGuess <- reactiveValues(value = NULL)
    trueMean <- reactiveValues(value = NULL)
    
    observeEvent({
        input$sampleSize
        input$retryButton
        }, {
        plotData$data <- round(runif(input$sampleSize, min = 1, max = 10), 0)
        meanGuess$value <- NULL
        trueMean$value <- NULL
        },
        ignoreNULL = FALSE
    )

    observeEvent(input$dotClick, {
        meanGuess$value <- round(input$dotClick$x, 2)
    })
    
    observeEvent(input$checkButton, {
        trueMean$value <- mean(plotData$data)
    })    

    output$dotPlot <- renderPlot({
        # draw the dotplot
        plotPoints <- data.frame(data = plotData$data)

        dotPlot <- ggplot(data.frame(plotPoints)) +
            geom_dotplot(aes(x = data), method = "histodot", binwidth = 1, dotsize = 0.7) +
            scale_y_continuous(NULL, breaks = NULL) +
            labs(title = "Random Sample Distribution - Where is the mean?",
                 x = "Sample Values")
    
        if (!is.null(meanGuess$value)) {
            dotPlot <- dotPlot +
                geom_vline(aes(xintercept = meanGuess$value),
                           size = 1, color = "blue", alpha = 0.8) +
                geom_vline(aes(xintercept = meanGuess$value),
                           size = 12, color = "blue", alpha = 0.2)
        }
        
        if (!is.null(trueMean$value)) {
            dotPlot <- dotPlot +
                geom_vline(aes(xintercept = trueMean$value),
                           size = 1, color = "red", alpha = 1.0)
        }
    
        dotPlot
    }, height = "auto")
    
    output$statusMessage <- renderText({
        if (!is.null(trueMean$value) && !is.null(meanGuess$value)) {
            errorValue = abs((trueMean$value - meanGuess$value) / 
                                 (max(plotData$data) - min(plotData$data)))
            outputMessage <- paste0("Your guess of the mean was within ",
                                    round(errorValue * 100, 1),
                                    "% of the true mean.")
            
            if (errorValue < 0.02) {
                outputMessage <- paste0(outputMessage, " That's a pretty darn good guess!")
            } else if (errorValue < 0.05) {
                outputMessage <- paste0(outputMessage, " Not too bad. That's pretty close!")
            } else if (errorValue < 0.10) {
                outputMessage <- paste0(outputMessage, " That's a reasonable effort!")
            } else {
                outputMessage <- paste0(outputMessage, " Try again. You will get there!")
            }

        } else {
            errorValue = NULL
            outputMessage <- "Click on the graph to guess the location of the mean of the
                              distribution. Then check your answer using the check button."
        }
        
        outputMessage
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
