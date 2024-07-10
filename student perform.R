
#Load the Data
#First, ensure your data is loaded into R.
#Importing necessary Libraries
library(ggplot2)
library(GGally)
library(dplyr)
library(stats)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(outliers)
library(dlookr)
library(car)

#Load the Dataset
data = read.csv("C:/Users/SURYA/Documents/Project Documents/Student_Performance.csv")


#Check the structure and summary of your dataset.
str(data)
summary(data)


#Data Preprocessing
#Handle categorical variables, missing values

# Convert categorical variables to factors
data$Extracurricular.Activities <- as.factor(data$Extracurricular.Activities)

# Handle missing values (if any)
data <- na.omit(data)


# Load libraries
library(ggplot2)
library(gridExtra)
#ggplot2 package to create various plots for the numeric values in your dataset.



#Plot Histograms
#To visualize the distribution of each numeric variable


# Plot histogram for Hours Studied
p1 <- ggplot(data, aes(x = Hours.Studied)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ggtitle("Histogram of Hours Studied")

# Plot histogram for Previous Scores
p2 <- ggplot(data, aes(x = Previous.Scores)) + 
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  ggtitle("Histogram of Previous Scores")

# Plot histogram for Sleep Hours
p3 <- ggplot(data, aes(x = Sleep.Hours)) + 
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  ggtitle("Histogram of Sleep Hours")

# Plot histogram for Sample Question Papers Practiced
p4 <- ggplot(data, aes(x = Sample.Question.Papers.Practiced)) + 
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  ggtitle("Histogram of Sample Question Papers Practiced")

# Scatter plot for Hours Studied vs Performance Index
p5 <- ggplot(data, aes(x = Hours.Studied, y = Performance.Index)) + 
  geom_point(color = "blue") +
  ggtitle("Scatter Plot: Hours Studied vs Performance Index")

# Scatter plot for Previous Scores vs Performance Index
p6 <- ggplot(data, aes(x = Previous.Scores, y = Performance.Index)) + 
  geom_point(color = "green") +
  ggtitle("Scatter Plot: Previous Scores vs Performance Index")

# Scatter plot for Sleep Hours vs Performance Index
p7 <- ggplot(data, aes(x = Sleep.Hours, y = Performance.Index)) + 
  geom_point(color = "red") +
  ggtitle("Scatter Plot: Sleep Hours vs Performance Index")

# Scatter plot for Sample Question Papers Practiced vs Performance Index
p8 <- ggplot(data, aes(x = Sample.Question.Papers.Practiced, y = Performance.Index)) + 
  geom_point(color = "purple") +
  ggtitle("Scatter Plot: Sample Question Papers Practiced vs Performance Index")

# Box plot for Hours Studied
p9 <- ggplot(data, aes(y = Hours.Studied)) + 
  geom_boxplot(fill = "blue") +
  ggtitle("Box Plot of Hours Studied")

# Box plot for Previous Scores
p10 <- ggplot(data, aes(y = Previous.Scores)) + 
  geom_boxplot(fill = "green") +
  ggtitle("Box Plot of Previous Scores")

# Box plot for Sleep Hours
p11 <- ggplot(data, aes(y = Sleep.Hours)) + 
  geom_boxplot(fill = "red") +
  ggtitle("Box Plot of Sleep Hours")

# Box plot for Sample Question Papers Practiced
p12 <- ggplot(data, aes(y = Sample.Question.Papers.Practiced)) + 
  geom_boxplot(fill = "purple") +
  ggtitle("Box Plot of Sample Question Papers Practiced")

# Arrange the plots in a grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol = 4)




######## Correlation Analysis##########

# Load the library
library(ggcorrplot)

# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(corrplot)
library(ggcorrplot)


# Ensure categorical variables are factors

data$Extracurricular.Activities <- as.factor(data$Extracurricular.Activities)

# Select numeric columns for correlation analysis

numeric_data <- data[, c("Hours.Studied","Previous.Scores","Sleep.Hours","Sample.Question.Papers.Practiced", "Performance.Index")]

# Calculate the correlation matrix

cor_matrix <- cor(numeric_data, use = "complete.obs")

# Print the correlation matrix

print(cor_matrix)

# Plot the correlation matrix using ggcorrplot

ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           tl.cex = 12, 
           tl.col = "black", 
           title = "Correlation Matrix", 
           ggtheme = theme_minimal())

#Overall, the strongest predictor of the Performance Index in this dataset appears 
#to be Previous Scores, followed by Hours Studied, while Sleep Hours and 
#Sample Question Papers Practiced show minimal correlations with performance.


#######Split the Data#####

install.packages("caTools")

library(caTools)
# Set a seed for reproducibility
set.seed(123)

# Split the data
split <- sample.split(data$Performance.Index, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


#Build the Model
#Create and train the linear regression model.

model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = train_data)

# Summary of the model

summary(model)

#The model predicts that each additional hour studied increases the Performance Index by
#approximately 2.86 units, and each point increase in previous scores adds about 1.02 units.

#Participation in extracurricular activities boosts the Performance Index by 
#approximately 0.61 units, while each additional hour of sleep and set of practiced 
#question papers add around 0.47 and 0.21 units, respectively.

#Overall, the model explains 98.89% of the Performance Index variation, 
#with a residual standard error of approximately 2.023 units, 
#indicating strong predictive power and significant contributions from all factors.



# Predict on the testing data
predictions <- predict(model, newdata = test_data)

# Calculate metrics (e.g., RMSE, R-squared) to evaluate model performance
rmse <- sqrt(mean((predictions - test_data$Performance.Index)^2))
rsquared <- cor(predictions, test_data$Performance.Index)^2

# Print evaluation metrics
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsquared, "\n")


#your model shows high accuracy (low RMSE) and a strong ability to explain the 
#variability in the Performance Index (high R-squared), which is desirable for 
#predictive modeling. This indicates that the chosen predictors are highly effective 
#in predicting the Performance Index based on the provided data.



# Create a data frame for actual vs predicted values
results <- data.frame(Actual = test_data$Performance.Index, Predicted = predictions)

# Plot actual vs predicted values
plot(results$Actual, results$Predicted,
     xlab = "Actual Performance Index",
     ylab = "Predicted Performance Index",
     main = "Actual vs Predicted")

# Add a diagonal line for reference (perfect predictions)
abline(0, 1, col = "red")

# Add legend and grid
legend("topright", legend = "Predicted = Actual", col = "red", lty = 1, cex = 0.8)
grid()


#In simple terms, a tight cluster around the red line means your model is doing a 
#good job of predicting the Performance Index. It shows how close your predictions 
#are to the real outcomes.
