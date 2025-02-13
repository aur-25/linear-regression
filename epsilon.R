library(shiny)

# Define UI
ui <- fluidPage(
    titlePanel("Estimate Probability P(|X̄ - μ| > ε)"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("n", "Sample Size (n):", value = 1000, min = 1),
            numericInput("k", "Number of Samples (k):", value = 1000, min = 1),
            numericInput("s", "Standard Deviation (s):", value = 10, min = 0, step = 0.1),
            numericInput("epsilon", "Epsilon (ε):", value = 1, min = 0, step = 0.1),
            actionButton("compute", "Compute Probability")
        ),
        
        mainPanel(
            h3("Result:"),
            verbatimTextOutput("probability"),
            plotOutput("histogram")
        )
    )
)

# Define Server
server <- function(input, output) {
    observeEvent(input$compute, {
        n <- input$n
        k <- input$k
        s <- input$s
        epsilon <- input$epsilon
        mu <- 50  # Population mean

        # Generate k sample means
        sample_means <- replicate(k, mean(rnorm(n, mean = mu, sd = s)))

        # Compute probability estimate P(|X̄ - μ| > ε)
        prob <- mean(abs(sample_means - mu) > epsilon)

        # Output probability result
        output$probability <- renderText({ 
            paste("Estimated Probability:", round(prob, 4)) 
        })
        
        # Plot histogram of sample means
        output$histogram <- renderPlot({
            hist(sample_means, breaks = 30, probability = TRUE, col = "lightblue",
                 main = "Distribution of Sample Means",
                 xlab = "Sample Mean", ylab = "Density")
            curve(dnorm(x, mean = mu, sd = s / sqrt(n)), 
                  col = "red", lwd = 2, add = TRUE)
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
