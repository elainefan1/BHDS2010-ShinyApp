#
# Shiny App – Exploring Hospital Billing Predictors
#

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

# ================================
# Load and clean dataset
# ================================

df_shinyapp <- read_csv("healthcare_dataset.csv")  

# 1) Rename columns with spaces to clean names
df <- df_shinyapp %>%
  rename(
    billing_amount     = `Billing Amount`,
    medical_condition  = `Medical Condition`,
    insurance_provider = `Insurance Provider`,
    date_admission     = `Date of Admission`
  ) %>%
  # 2) Change the types
  mutate(
    billing_amount     = as.numeric(billing_amount),
    Gender             = as.factor(Gender),
    medical_condition  = as.factor(medical_condition),
    insurance_provider = as.factor(insurance_provider),
    date_admission     = as.Date(date_admission)
  ) %>%
  # 3) Create bins for admissions histogram
  mutate(
    month_bin   = floor_date(date_admission, "month"),
    quarter_bin = floor_date(date_admission, "quarter"),
    year_bin    = floor_date(date_admission, "year")
  )

# =========================================
# UI
# =========================================
options(shiny.maxRequestSize = 50 * 1024^2)
ui <- navbarPage(
  
  "Exploring What Makes Hospital Bills Expensive",
  
  tabPanel(
    sidebarLayout(
      
      sidebarPanel(
        
        h4("Admissions Time Scale"),
        selectInput(
          inputId = "time_bin",
          label = "Choose admissions time scale:",
          choices = c("Monthly", "Quarterly", "Yearly"),
          selected = "Monthly"
        ),
        
        hr(),
        
        h4("Explore Billing by Predictors"),
        
        selectInput(
          "gender_select",
          "Select Gender:",
          choices = sort(unique(df$Gender))
        ),
        
        selectInput(
          "condition_select",
          "Select Medical Condition:",
          choices = sort(unique(df$medical_condition))
        ),
        
        selectInput(
          "provider_select",
          "Select Insurance Provider:",
          choices = sort(unique(df$insurance_provider))
        )
      ),
      
      mainPanel(
        
        h3("Admissions Histogram"),
        plotOutput("admissions_plot"),
        
        hr(),
        
        h3("Overall Billing Distribution"),
        plotOutput("billing_plot"),
        
        hr(),
        
        h3("Billing by Gender"),
        plotOutput("gender_plot"),
        
        hr(),
        
        h3("Billing by Medical Condition"),
        plotOutput("condition_plot"),
        
        hr(),
        
        h3("Billing by Insurance Provider"),
        plotOutput("provider_plot")
      )
    )
  ),
  
  tabPanel(
    'Data Analysis Summaries',
    fluidPage(
      h3("Summary Statistics"),
      verbatimTextOutput("summary_table")
    )
  )
)

# =========================================
# SERVER
# =========================================

server <- function(input, output) {
  
  # ------------------------------------
  # 1. Admissions Histogram (Elaine)
  # ------------------------------------
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
  
  # ------------------------------------
  # 2. Billing Histogram (Overall)
  # ------------------------------------
  output$billing_plot <- renderPlot({
    ggplot(df, aes(x = billing_amount)) +
      geom_histogram(binwidth = 500, fill = "purple", color = "black", alpha = 0.7) +
      theme_bw() +
      xlab("Billing Amount") +
      ylab("Count") +
      ggtitle("Distribution of Billing Amounts")
  })
  
  # ------------------------------------
  # 3. Josh – Key Predictors of Billing Amount
  # ------------------------------------
  
  # Billing by Gender
  output$gender_plot <- renderPlot({
    df_gender <- df %>% filter(Gender == input$gender_select)
    
    ggplot(df_gender, aes(x = billing_amount)) +
      geom_histogram(binwidth = 500, fill = "darkorange", color = "black") +
      theme_minimal() +
      ggtitle(paste("Billing Distribution for Gender:", input$gender_select)) +
      xlab("Billing Amount") + ylab("Count")
  })
  
  # Billing by Medical Condition
  output$condition_plot <- renderPlot({
    df_cond <- df %>% filter(medical_condition == input$condition_select)
    
    ggplot(df_cond, aes(x = billing_amount)) +
      geom_histogram(binwidth = 500, fill = "seagreen", color = "black") +
      theme_minimal() +
      ggtitle(paste("Billing for Condition:", input$condition_select)) +
      xlab("Billing Amount") + ylab("Count")
  })
  
  # Billing by Insurance Provider
  output$provider_plot <- renderPlot({
    df_prov <- df %>% filter(insurance_provider == input$provider_select)
    
    ggplot(df_prov, aes(x = billing_amount)) +
      geom_histogram(binwidth = 500, fill = "dodgerblue4", color = "black") +
      theme_minimal() +
      ggtitle(paste("Billing for Provider:", input$provider_select)) +
      xlab("Billing Amount") + ylab("Count")
  })
  
  output$summary_table <- renderPrint({summary(df)}) #outputting the summary statistics
}

# Run the application 
shinyApp(ui = ui, server = server)

