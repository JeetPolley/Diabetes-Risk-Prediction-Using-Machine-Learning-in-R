# Load data
diabetes_data <- read.csv("C:\\Users\\HP\\Downloads\\diabetes.csv")  
head(diabetes_data)  

 

# Check imbalance (critical for model accuracy)
table(diabetes_data$Outcome)  # 500 non-diabetic vs 268 diabetic

library(ggplot2)  
ggplot(diabetes_data, aes(x = factor(Outcome), y = Glucose, fill = factor(Outcome))) +  
  geom_boxplot() +  
  labs(title = "Higher Glucose = Higher Diabetes Risk")  
# Load libraries
library(tidyverse)  
library(caret)  

library(pROC)  


# Check structure
glimpse(diabetes_data)  

# Summary statistics
summary(diabetes_data)  

# Check missing values
sum(is.na(diabetes_data)) 


# Handle zeros (some features like Glucose cannot be zero)
diabetes_clean <- diabetes_data %>%
  mutate(
    Glucose = ifelse(Glucose == 0, median(Glucose, na.rm = TRUE), Glucose),
    BloodPressure = ifelse(BloodPressure == 0, median(BloodPressure, na.rm = TRUE), BloodPressure),
    BMI = ifelse(BMI == 0, median(BMI, na.rm = TRUE), BMI)
  )

# Normalize numerical features
preproc <- preProcess(diabetes_clean, method = c("center", "scale"))
diabetes_normalized <- predict(preproc, diabetes_clean)  

# Split into train & test sets
set.seed(123)  
train_index <- createDataPartition(diabetes_normalized$Outcome, p = 0.8, list = FALSE)  
train_data <- diabetes_normalized[train_index, ]  
test_data <- diabetes_normalized[-train_index, ] 

summary(diabetes_normalized$Outcome)
range(diabetes_normalized$Outcome, na.rm = TRUE)  # Should be 0-1
diabetes_normalized$Outcome <- ifelse(diabetes_normalized$Outcome > 0.5, 1, 0)

# Train model
model <- glm(diabetes_normalized$Outcome ~ ., data = diabetes_normalized, family = binomial)  

# Predict on test set
predictions <- predict(model, test_data, type = "response")  
predicted_class <- ifelse(predictions > 0.5, 1, 0)  

# Evaluate model
confusionMatrix(factor(predicted_class), factor(test_data$Outcome))  
roc_curve <- roc(test_data$Outcome, predictions)  
plot(roc_curve, main = "ROC Curve")  
auc(roc_curve)  # Higher AUC = Better model  

library(vip)  
vip(model, num_features = 5)  
#calculator
library(shiny)  

ui <- fluidPage(
  titlePanel("Diabetes Risk Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("glucose", "Glucose Level:", 100),
      numericInput("bmi", "BMI:", 25),
      actionButton("predict", "Calculate Risk")
    ),
    mainPanel(
      textOutput("risk_score"),
      plotOutput("risk_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      Pregnancies = 1, Glucose = input$glucose, BloodPressure = 70,
      SkinThickness = 20, Insulin = 80, BMI = input$bmi,
      DiabetesPedigreeFunction = 0.5, Age = 30
    )
    risk <- predict(model, new_data, type = "response")
    output$risk_score <- renderText(paste("Risk of Diabetes:", round(risk * 100, 1), "%"))
  })
}

shinyApp(ui, server)


#SHAP

if (!require("fastshap")) install.packages("fastshap")  # For SHAP calculations
if (!require("ggplot2")) install.packages("ggplot2")    # For visualizations
library(fastshap)
library(ggplot2)  

summary(diabetes_normalized$Outcome)
range(diabetes_normalized$Outcome, na.rm = TRUE)  # Should be 0-1
diabetes_normalized$Outcome <- ifelse(diabetes_normalized$Outcome > 0.5, 1, 0)

# Train model
model <- glm(diabetes_normalized$Outcome ~ ., data = diabetes_normalized, family = binomial)  








# Create prediction function for SHAP
pred_fun <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}


# Select features of interest (remove Outcome column)
features <- train_data[, c("Glucose", "BMI", "Age")]
# Check what variables the model expects
names(model$coefficients)

# Get the EXACT features used in model training
required_features <- names(model$coefficients)[-1]  # Remove intercept

# Check if all exist in your features dataframe
all(required_features %in% names(features))

# Option A: If you intentionally removed features
features <- train_data[, required_features]  # Use same columns as training

# Option B: If names differ
features <- features %>% 
  rename(Pregnancies = pregnancy_count)  # Example name correction
shap_values <- fastshap::explain(
  model,
  X = train_data[, required_features],  # Use correct features
  pred_wrapper = pred_fun,
  nsim = 50
)



# Add row IDs for plotting
shap_values$ID <- seq_len(nrow(shap_values))

library(tidyr)
shap_importance <- as.data.frame(shap_values) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -ID, names_to = "Feature", values_to = "SHAP") %>%
  group_by(Feature) %>%
  summarise(Mean_SHAP = mean(abs(SHAP))) %>%
  arrange(desc(Mean_SHAP))

ggplot(shap_importance, aes(x = reorder(Feature, Mean_SHAP), y = Mean_SHAP)) +
  geom_col(fill = "#3498db") +
  coord_flip() +
  labs(title = "SHAP Feature Importance",
       x = "",
       y = "Mean |SHAP Value|") +
  theme_minimal()





 