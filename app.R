#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Exploring What Makes Hospital Bills Expensive"),
    sidebarLayout(
      
      sidebarPanel( #creating a side panel to choose between displaying the histogram in monthly, quarterly, or yearly bins
        selectInput(
          inputId = "time_bin",
          label = "Choose admissions time scale:",
          choices = c("Monthly", "Quarterly", "Yearly"),
          selected = "Monthly"
        )
      ),
        # Show a plot of the generated distribution
      mainPanel(
        h3("Admissions Histogram"),
        plotOutput("admissions_plot"),
        hr(),
        h3("Billing Amount Distribution"),
        plotOutput("billing_plot")
      )
    )
)

# Define server logic required 
server <- function(input, output) {

  admissions_data <- reactive({
    if (input$time_bin == "Monthly") {
      df$month_bin
    } else if (input$time_bin == "Quarterly") {
      df$quarter_bin
    } else {
      df$year_bin
    }
  })
  
  
  output$admissions_plot <- renderPlot({
    
    ggplot(data.frame(x = admissions_data()), aes(x = x)) +
      geom_histogram(stat = "count", fill = "steelblue", color = "black") +
      theme_bw() +
      xlab("Admission Date (Binned)") +
      ylab("Number of Admissions") +
      ggtitle(paste("Admissions –", input$time_bin)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$billing_plot <- renderPlot({
    
    ggplot(df, aes(x = billing_amount)) +
      geom_histogram(binwidth = 500, fill = "purple", color = "black", alpha = 0.7) +
      theme_bw() +
      xlab("Billing Amount") +
      ylab("Count") +
      ggtitle("Distribution of Billing Amounts")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
