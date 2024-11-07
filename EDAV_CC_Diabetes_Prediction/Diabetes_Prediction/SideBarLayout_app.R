# Load necessary libraries
library(shiny)
library(caret)
library(randomForest)

# Load the pre-trained model and dataset for visualizations
model <- readRDS("C:\\Sharvay\\My_Files\\Notes\\Columbia_MSDS\\EDAV\\Assignments\\Community Contribution/diabetes_model.rds")
data <- read.csv("C:\\Sharvay\\My_Files\\Notes\\Columbia_MSDS\\EDAV\\Assignments\\Community Contribution/diabetes_prediction_dataset.csv")

# Define a plotting function
plot_distribution <- function(var_data, user_input, var_name, is_numeric = TRUE) {
  if (is_numeric) {
    hist(var_data, breaks = 20, col = "lightblue", main = paste(var_name, "Distribution"),
         xlab = var_name, xlim = range(var_data))
    abline(v = user_input, col = "red", lwd = 2)
    legend("topright", legend = paste("Your Input:", user_input), col = "red", lwd = 2)
  } else {
    barplot(table(var_data), col = "lightblue", main = paste(var_name, "Distribution"),
            ylab = "Count")
    text(x = which(levels(var_data) == user_input), y = max(table(var_data)) * 0.9,
         labels = "Your Input", col = "red", cex = 1.2, font = 2)
  }
}

# Define UI for the application
ui <- fluidPage(
  titlePanel("Diabetes Prediction App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", min = 0, max = 100, value = 50),
      selectInput("gender", "Gender:", choices = c("Male", "Female")),
      numericInput("bmi", "BMI:", min = 10, max = 60, value = 25),
      selectInput("hypertension", "Hypertension:", choices = c("No" = 0, "Yes" = 1)),
      selectInput("heart_disease", "Heart Disease:", choices = c("No" = 0, "Yes" = 1)),
      selectInput("smoking_history", "Smoking History:", choices = c("No Info" = "No Info", "Current" = "Current", "Former" = "Former", "Never" = "Never")),
      numericInput("HbA1c_level", "HbA1c Level:", min = 3, max = 20, value = 5.5),
      numericInput("blood_glucose_level", "Blood Glucose Level:", min = 50, max = 250, value = 120),
      actionButton("predict", "Predict Diabetes")
    ),
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("prediction"),
      h3("Input Comparisons"),
      
      # Display plots in two columns
      fluidRow(
        column(6, plotOutput("age_plot")),
        column(6, plotOutput("bmi_plot"))
      ),
      fluidRow(
        column(6, plotOutput("glucose_plot")),
        column(6, plotOutput("HbA1c_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Render prediction when the button is clicked
  observeEvent(input$predict, {
    # Prepare input data frame for prediction
    input_data <- data.frame(
      age = as.numeric(input$age),
      gender = factor(input$gender, levels = c("Male", "Female")),
      bmi = as.numeric(input$bmi),
      hypertension = factor(input$hypertension, levels = c("0", "1")),
      heart_disease = factor(input$heart_disease, levels = c("0", "1")),
      smoking_history = factor(input$smoking_history, levels = c("No Info", "Current", "Former", "never")),
      HbA1c_level = as.numeric(input$HbA1c_level),
      blood_glucose_level = as.numeric(input$blood_glucose_level)
    )
    
    # Run prediction and provide interpretation
    if (nrow(input_data) > 0 && all(!is.na(input_data))) {
      result <- predict(model, newdata = input_data)
      interpretation <- ifelse(result == 1, "Has Diabetes", "No Diabetes")
      output$prediction <- renderText({
        paste("Diabetes Prediction:", interpretation)
      })
    } else {
      output$prediction <- renderText({
        "Invalid input data. Please check your entries."
      })
    }
  })
  
  
  # Generate individual plots using the plot_distribution function
  output$age_plot <- renderPlot({
    plot_distribution(data$age, input$age, "Age")
  })
  
  output$bmi_plot <- renderPlot({
    plot_distribution(data$bmi, input$bmi, "BMI")
  })
  
  output$glucose_plot <- renderPlot({
    plot_distribution(data$blood_glucose_level, input$blood_glucose_level, "Blood Glucose Level")
  })
  # 
  # output$smoking_history_plot <- renderPlot({
  #   plot_distribution(data$smoking_history, input$smoking_history, "Smoking History", is_numeric = FALSE)
  # })
  
  output$HbA1c_plot <- renderPlot({
    plot_distribution(data$HbA1c_level, input$HbA1c_level, "HbA1c Level")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
