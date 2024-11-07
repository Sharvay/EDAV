# Load necessary libraries
library(shiny)
library(shinythemes)  # For themes
library(caret)
library(randomForest)
library(ggplot2)

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
  theme = shinytheme("flatly"),  # Applying a theme
  
  # Title Panel
  titlePanel("Diabetes Prediction & Analysis Tool"),
  
  # Input Section for Prediction
  fluidRow(
    column(
      12,
      div(
        style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
        h3("Diabetes Prediction"),
        fluidRow(
          column(4, numericInput("age", "Age:", min = 0, max = 100, value = 50)),
          column(4, selectInput("gender", "Gender:", choices = c("Male", "Female"))),
          column(4, numericInput("bmi", "BMI:", min = 10, max = 60, value = 25)),
          column(4, selectInput("hypertension", "Hypertension:", choices = c("No" = 0, "Yes" = 1))),
          column(4, selectInput("heart_disease", "Heart Disease:", choices = c("No" = 0, "Yes" = 1))),
          column(4, selectInput("smoking_history", "Smoking History:", 
                                choices = c("No Info" = "No Info", "Current" = "current", "Former" = "former", "Never" = "never"))),
          column(4, numericInput("HbA1c_level", "HbA1c Level:", min = 3, max = 20, value = 5.5)),
          column(4, numericInput("blood_glucose_level", "Blood Glucose Level:", min = 50, max = 250, value = 120)),
          column(12, actionButton("predict", "Predict Diabetes", class = "btn-primary"))
        ),
        verbatimTextOutput("prediction")
      )
    )
  ),
  
  # Analysis Section
  fluidRow(
    column(
      12,
      div(
        style = "background-color: #eaf1f8; padding: 20px; border-radius: 5px;",
        h3("Input Comparisons & Data Analysis"),
        
        # Feature Importance Plot
        plotOutput("feature_importance"),
        
        # Individual variable comparisons
        fluidRow(
          column(6, plotOutput("age_plot")),
          column(6, plotOutput("bmi_plot"))
        ),
        
        fluidRow(
          column(6, plotOutput("glucose_plot")),
          column(6, plotOutput("HbA1c_plot"))
        ),
        
        # Additional Analysis Section
        h4("Additional Analysis"),
        fluidRow(
          column(6, plotOutput("age_diabetes_dist_plot")),
          column(6, plotOutput("bmi_glucose_scatter_plot"))
        ),
        
        fluidRow(
          column(6, plotOutput("hypertension_diabetes_plot")),
          column(6, plotOutput("gender_diabetes_plot"))
        )
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
      smoking_history = factor(input$smoking_history, levels = c("No Info", "current", "former", "never")),
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
  
  # Plot feature importance
  output$feature_importance <- renderPlot({
    varImpPlot(model$finalModel, main = "Feature Importance for the ML Model")
  })
  
  # Generate individual plots using the `plot_distribution` function
  output$age_plot <- renderPlot({
    plot_distribution(data$age, input$age, "Age")
  })
  
  output$bmi_plot <- renderPlot({
    plot_distribution(data$bmi, input$bmi, "BMI")
  })
  
  output$glucose_plot <- renderPlot({
    plot_distribution(data$blood_glucose_level, input$blood_glucose_level, "Blood Glucose Level")
  })
  
  output$HbA1c_plot <- renderPlot({
    plot_distribution(data$HbA1c_level, input$HbA1c_level, "HbA1c Level")
  })
  
  # Age distribution by diabetes outcome
  output$age_diabetes_dist_plot <- renderPlot({
    ggplot(data, aes(x = age, fill = factor(diabetes))) +
      geom_density(alpha = 0.5) +
      labs(title = "Age Distribution by Diabetes Outcome", fill = "Diabetes Status") +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "salmon")) +
      theme_minimal()
  })
  
  # BMI vs Blood Glucose Scatter Plot by Diabetes Outcome
  output$bmi_glucose_scatter_plot <- renderPlot({
    ggplot(data, aes(x = bmi, y = blood_glucose_level, color = factor(diabetes))) +
      geom_point(alpha = 0.6) +
      labs(title = "BMI vs Blood Glucose Level by Diabetes Status", x = "BMI", y = "Blood Glucose Level", color = "Diabetes Status") +
      scale_color_manual(values = c("0" = "blue", "1" = "red")) +
      theme_minimal()
  })
  
  # Hypertension and Diabetes Bar Plot
  output$hypertension_diabetes_plot <- renderPlot({
    ggplot(data, aes(x = factor(hypertension), fill = factor(diabetes))) +
      geom_bar(position = "fill") +
      labs(title = "Hypertension Proportion by Diabetes Status", x = "Hypertension", y = "Proportion", fill = "Diabetes Status") +
      scale_fill_manual(values = c("0" = "lightgreen", "1" = "orange")) +
      theme_minimal()
  })
  
  # Gender-based Diabetes Outcome Distribution
  output$gender_diabetes_plot <- renderPlot({
    ggplot(data, aes(x = gender, fill = factor(diabetes))) +
      geom_bar(position = "fill") +
      labs(title = "Gender Distribution by Diabetes Outcome", x = "Gender", y = "Proportion", fill = "Diabetes Status") +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "pink")) +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
