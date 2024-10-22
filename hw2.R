if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("https://drive.google.com/uc?export=download&id=1ulO3XuH_Xl52qW2kuxXmYclxOPeWv_bI")

ui <- fluidPage(
  titlePanel("Mobile Device Usage and User Behavior"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("os", "Select Operating System:", 
                  choices = unique(data$Operating.System), 
                  selected = "iOS"),
      
      sliderInput("age", "Select Age Range:", 
                  min = min(data$Age), 
                  max = max(data$Age), 
                  value = c(20, 40)),
      
      selectInput("gender", "Select Gender:", 
                  choices = unique(data$Gender), 
                  selected = "Male"),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      plotOutput("usagePlot"),
      plotOutput("batteryPlot"),
      textOutput("summaryText")
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression to filter the data based on user input
  filtered_data <- reactive({
    data %>%
      filter(Operating.System == input$os, 
             Age >= input$age[1], Age <= input$age[2], 
             Gender == input$gender)
  })
  
  # Plot showing app usage time based on filtered data
  output$usagePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Age, y = `App.Usage.Time..min.day.`, color = Device.Model)) +
      geom_point() +
      labs(title = "App Usage Time by Age and Device Model", 
           x = "Age", 
           y = "App Usage Time (min/day)") +
      theme_minimal()
  })
  
  # Plot showing battery drain based on filtered data
  output$batteryPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Age, y = `Battery.Drain..mAh.day.`, color = `Operating.System`)) +
      geom_point() +
      labs(title = "Battery Drain by Age and OS", 
           x = "Age", 
           y = "Battery Drain (mAh/day)") +
      theme_minimal()
  })
  
  # Summary text to provide context about the filtered data
  output$summaryText <- renderText({
    avg_usage <- mean(filtered_data()$`App.Usage.Time..min.day.`, na.rm = TRUE)
    avg_battery <- mean(filtered_data()$`Battery.Drain..mAh.day.`, na.rm = TRUE)
    
    paste("Average App Usage Time:", round(avg_usage, 2), "min/day",
          "| Average Battery Drain:", round(avg_battery, 2), "mAh/day")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "os", selected = "iOS")
    updateSliderInput(session, "age", value = c(20, 40))
    updateSelectInput(session, "gender", selected = "Male")
  })
}
shinyApp(ui = ui, server = server)
