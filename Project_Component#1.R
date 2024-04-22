library(rpart)
library(rpart.plot)

# Set the working directory
setwd("C:/Users/Kapad/OneDrive/Desktop/Rutgers/R Related/Project")

# Read the training data
train_data <- read.csv("EarningsTrain.csv")

# Remove outliers from the training data
remove_outliers <- function(data, num_vars, threshold = 3) {
  for (var in num_vars) {
    z_scores <- abs(scale(data[[var]]))
    data <- data[z_scores < threshold, ]
  }
  return(data)
}

num_vars <- c("Number_Of_Professional_Connections", "GPA", "Earnings", 
              "Graduation_Year", "Height", "Number_Of_Credits", "Number_Of_Parking_Tickets")

# First round of outlier removal
train_data <- remove_outliers(train_data, num_vars)

# Train the decision tree model after the first round of outlier removal
model_tree <- rpart(Earnings ~ GPA + Number_Of_Professional_Connections + Major, 
                    data = train_data, 
                    control = rpart.control(cp = 0.00003, minsplit = 13))

# Predict earnings on the training set
train_data$predEarnings <- predict(model_tree, newdata = train_data)

# Calculate MSE on the training set after the first round of outlier removal
MSE <- mean((train_data$Earnings - train_data$predEarnings)^2)
print(paste("Training MSE after first round of outlier removal:", MSE))

rpart.plot(model_tree)

# Write the test data with predictions to a new CSV file
write.csv(train_data, "Earnings_Test.csv", row.names = FALSE)

