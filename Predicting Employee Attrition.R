# Get Current Directory
getwd()

# Set Directory
setwd('D:\\Rushi\\Edubridge\\R\\Project')

# Load Data
data <- read.csv('Info.csv')
View(data)

# Install ggplot2 if not already installed
if(!require(ggplot2)) {
  install.packages('ggplot2', dependencies = TRUE)
  library(ggplot2)
}

# Verify the correct names of the columns
str(data)

# Convert Dateofjoining and LastWorkingDate to Date format
data$Dateofjoining <- as.Date(data$Dateofjoining, format = "%Y-%m-%d")
data$LastWorkingDate <- as.Date(data$LastWorkingDate, format = "%Y-%m-%d")

# Calculate total number of working days and add it as a new column 'tenurity'
data$tenurity <- ifelse(is.na(data$LastWorkingDate), 
                        as.numeric(Sys.Date() - data$Dateofjoining),
                        as.numeric(data$LastWorkingDate - data$Dateofjoining))

# Add a column for Attrition
data$Attrition <- ifelse(is.na(data$LastWorkingDate), "No", "Yes")

# Remove the last working date column
data$LastWorkingDate <- NULL

# Handle missing values
# Replace NA with mean for numerical columns and mode for categorical columns
for(col in colnames(data)) {
  if(is.numeric(data[[col]])) {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  } else {
    data[[col]][is.na(data[[col]])] <- as.character(getmode(data[[col]]))
  }
}

# Define a function to get the mode of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Encode categorical variables
data$Gender <- as.factor(data$Gender)
data$City <- as.factor(data$City)
data$Education_Level <- as.factor(data$Education_Level)
data$Attrition <- as.factor(data$Attrition)

# Normalize numerical data to range 0-1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$Age <- normalize(data$Age)
data$Salary <- normalize(data$Salary)
data$Total Business Value <- normalize(data$Total Business Value)
data$Quarterly Rating <- normalize(data$Quarterly Rating)

View(data)


# Load ggplot2 library
library(ggplot2)

# Group data by employee_id and Attrition, summarizing with mean for numerical values and mode for categorical values
library(dplyr)

summary_data <- data %>%
  group_by(Emp_ID, Attrition) %>%
  summarise(
    City = getmode(City),
    Age = mean(Age),
    Gender = getmode(Gender),
    Education_Level = getmode(Education_Level),
    Quarterly.Rating = mean(Quarterly.Rating),
    Designation = getmode(Designation),
    tenurity = mean(tenurity),
    Salary = mean(Salary)
  )

# Define a function to plot attrition against different variables
plot_attrition <- function(var) {
  ggplot(data, aes_string(x = var, fill = "Attrition")) +
    geom_bar(position = "dodge") +
    labs(title = paste("Attrition by", var),
         x = var,
         y = "Count",
         fill = "Attrition") +
    theme_minimal()
}

# Plot attrition against City
plot_attrition("City")

# Plot attrition against Age
ggplot(data, aes(x = Age, fill = Attrition)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Attrition by Age",
       x = "Age",
       y = "Count",
       fill = "Attrition") +
  theme_minimal()

# Plot attrition against Gender
plot_attrition("Gender")

# Plot attrition against Education_Level
plot_attrition("Education_Level")

# Plot attrition against Quarterly.Rating
plot_attrition("Quarterly.Rating")

# Plot attrition against Designation
plot_attrition("Designation")

# Plot attrition against tenurity
plot_attrition("tenurity")

# Plot attrition against Salary
plot_attrition("Salary")



# Perform exploratory data analysis using ggplot2

# Pie Chart for Gender
ggplot(data, aes(x = "", fill = Gender)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  labs(title = "Pie Chart of Gender")

# Histogram for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

# Bar Chart for Education Level
ggplot(data, aes(x = Education_Level)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Bar Chart of Education Level", x = "Education Level", y = "Count")

# Line Plot for Salary over Time
ggplot(data, aes(x = as.Date(Dateofjoining), y = Salary, group = 1)) +
  geom_line(color = "purple") +
  labs(title = "Line Plot of Salary over Time", x = "Date of Joining", y = "Salary")

# Scatter Plot for Salary vs Total Business Value
ggplot(data, aes(x = Salary, y = `Total.Business.Value`)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Salary vs Total Business Value", x = "Salary", y = "Total Business Value")

# Box Plot for Quarterly Rating by Education Level
ggplot(data, aes(x = Education_Level, y = `Quarterly.Rating`, fill = Education_Level)) +
  geom_boxplot() +
  labs(title = "Box Plot of Quarterly Rating by Education Level", x = "Education Level", y = "Quarterly Rating")

# Density Plot for Salary
ggplot(data, aes(x = Salary, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Salary", x = "Salary", y = "Density")

# Violin Plot for Salary by Gender
ggplot(data, aes(x = Gender, y = Salary, fill = Gender)) +
  geom_violin() +
  labs(title = "Violin Plot of Salary by Gender", x = "Gender", y = "Salary")

# Error Bar for Quarterly Rating by Gender
ggplot(data, aes(x = Gender, y = `Quarterly.Rating`, color = Gender)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Error Bar of Quarterly Rating by Gender", x = "Gender", y = "Quarterly Rating")

# Correlation Heatmap
# Calculate correlation matrix
correlation_matrix <- cor(data[, sapply(data, is.numeric)])
library(reshape2)
melted_correlation <- melt(correlation_matrix)

# Plot the heatmap
ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable")

