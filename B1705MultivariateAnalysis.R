# ----- B1705: Week One | Multi-variate Analysis - Practical | 17.01.2024 -----

# ----- 1. Testing for Normality -----

##### 1.1. Assumption One: Normality #####
# Setting seed
set.seed(123) # Setting seed for reproducibility
# Dataset generation
data <- rnorm(100, mean = 50, sd = 10) # Generating a normally distributed dataset
# Creating a histogram of the data distribution
hist(data, main = "Histogram of Data Showing Normal Distribution", xlab = "Data values", border = "blue", col = "green")
# Q-Q plot of data for distribution
qqnorm(data)
qqline(data, col = "red")
# Shapiro-Wilk test; when p > 0.05, we can assume that the data is normally distributed
shapiro.test(data)
# Kolmogorov-Smirnov Test; again, p > 0.05 suggests normal distribution
ks.test(data, "pnorm", mean = mean(data), sd = sd(data))

# Code for a non-normally distributed dataset
# Set seed for reproducibility
set.seed(123)
# Generate a normally distributed vector
normal_data <- rnorm(1000, mean = 12, sd = 2)
# Apply an exponential transformation to create negative skew
# Since the exponential function is rapidly increasing, it stretches the right tail more than the left tail
neg_skew_data <- exp(-normal_data)
# Histogram
hist(neg_skew_data, main = "Histogram of Data", xlab = "Data", border = "blue", col = "green")
# Q-Q Plot of the non-normally distributed
qqnorm(neg_skew_data)
qqline(neg_skew_data, col = "red")
# Shapiro-Wilk test; p < 0.05, therefore suggesting a non-normal distribution of data
shapiro.test(neg_skew_data)
# Kolmogorov-Smirnov Test; again, p < 0.05, suggesting non-normal distribution of data
ks.test(data, "pnorm", mean = mean(neg_skew_data), sd = sd(neg_skew_data))

##### 1.1.2. Transformation #####
# When data is not normally distributed, we have options of how to deal with this such as transformation
log_data <- log(neg_skew_data)
# Now test log_data for normality as before
hist(log_data, main = "Histogram of log-transformed Data", xlab = "Data", border = "blue", col = "red")
# Shapiro-Wilk test on logarithmic version of data
shapiro.test(log_data)

# ----- 2. Testing for Linearity -----
##### 2.1. Testing Linear Line: Example #####
# Loading dataset
data(mtcars)
df <- mtcars

# Scatterplot for data
library(ggplot2)
ggplot(df, aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Scatter Plot with Linear Fit")

# Fitting a linear model
model <- lm(mpg ~ wt, data = df)

# Plot the residuals
plot(model$fitted.values, resid(model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


##### 2.2. Testing Quadratic Relationship #####
# Quadratic and Linear variables
# This script creates two variables with quadratic, rather than a linear, relationship.
# Set seed for reproducibility
set.seed(123)
# Create a sequence of numbers for the first variable
x <- seq(-10, 10, by = 0.1)
# Generate the second variable using a non-linear transformation to produce a Quadratic Relationship
y_quadratic <- 3 + 0.5 * x^2 + rnorm(length(x), mean = 0, sd = 2)
# Combine the data into a data frame
data_quadratic <- data.frame(x = x, y = y_quadratic)

# Plotting to visualise relationships
library(ggplot2)
# Plot for Quadratic Relationship
ggplot(data_quadratic, aes(x = x, y = y)) +
  geom_point() +
  ggtitle("Quadratic Relationship") +
  theme_minimal()

# Fit a linear model
model <- lm(x ~ y, data = data_quadratic)
# Plot the residuals
plot(model$fitted.values, resid(model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

##### 2.3. Addressing Non-Linearity #####
# Example of transforming variables in a non-linear relationship. The example below uses a non-linear statistical model
# Example: Using polynomial regression
model_poly <- lm(mpg ~ poly(wt, 2), data = df)
# Check the fit
ggplot(df, aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  ggtitle("Polynomial Regression Fit")


# ----- 3. Illustrating Homoscedasticity -----
# Creating Data
# Set seed for reproducibility
set.seed(123)
# Generate synthetic data
n <- 100
x <- rnorm(n, 50, 10)
y <- 2*x + rnorm(n, 0, 20) # This will create a bit of spread in the data
# Create a data frame
data <- data.frame(x, y)

# Fit a linear model
model <- lm(y ~ x, data = data)

# Plot residuals vs. fitted values
plot(model$fitted.values, resid(model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


# Install and load the lmtest package
library(lmtest)

# Perform Breusch-Pagan test; p < 0.05 suggests presence of heteroscedasticity, p > 0.05 suggests presence of homoscedasiticity
bptest(model)

# Ensuring your data is homoscedatic is vital, as it helps reveal that the assumption of your linear model is met. 

# ----- 4. Absence of Multi-collinearity 
# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
n <- 100
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- 2 * x1 + rnorm(n, mean = 0, sd = 5) # x2 is highly correlated with x1
y <- 5 + 3 * x1 - 2 * x2 + rnorm(n, mean = 0, sd = 2)

# Create a data frame
data <- data.frame(x1, x2, y)

# Plot 1; suggests x1 and y have a linear-association
library(ggplot2)
ggplot(data, aes(x = x1, y = y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Scatter Plot")

# Plot 2; suggests x2 and y have a linear-association
library(ggplot2)
ggplot(data, aes(x = x2, y = y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Scatter Plot")

# Plot 3; problematically suggests that x1 and x2 also have a linear-association
library(ggplot2)
ggplot(data, aes(x = x1, y = x2)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Scatter Plot")

# Here, we have to fit a linear model to this data; but we must be careful of the presence of multi-colllinearity
# Fit a linear model
model <- lm(y ~ x1 + x2, data = data)
# load car package for VIF
library(car)
# Calculate VIF
vif_results <- vif(model)
print(vif_results)

# High VIF values over 5 or 10 indicates high multi-collinearity between independent variables; as seen, x1 and x2 are highly correlated.
# High VIF suggests multi-collinearity here, and therefore our coefficients may not be reliable

# If multi-collinearity is an issue, you could consider the following steps:
  
#   Remove one of the correlated variables from your analysis.

#   Combine the correlated variables into a single variable. If they are highly correlated, they may represent the same underlying factor.

#   Use Partial Least Squares Regression or Principal Component Analysis. These methods are more robust against multicollinearity.


# ----- 5. Multi-variate Testing Assumptions: Practice -----

dataset <- read.csv('https://www.dropbox.com/scl/fi/i6g8ww62cxm56ofppr2x2/dataset.csv?rlkey=0910fampdxazuuhynxpu1x9a3&dl=1')


# ----- 5. Multi-variate Testing Assumptions: Practice -----

# Load the dataset
dataset <- read.csv('https://www.dropbox.com/scl/fi/i6g8ww62cxm56ofppr2x2/dataset.csv?rlkey=0910fampdxazuuhynxpu1x9a3&dl=1')

# Function to create histogram and Q-Q plots for each numeric column in the dataset
create_plots_for_numeric_columns <- function(dataset) {
  # Determine the numeric columns in the dataset
  numeric_columns <- sapply(dataset, is.numeric)
  # Iterate over the numeric columns generating plots
  for(column_name in names(numeric_columns[numeric_columns])) {
    # Extract column data
    column_data <- dataset[[column_name]]
    # Generate the histogram
    hist(column_data, main = paste("Histogram of", column_name, "Showing Normal Distribution"), xlab = column_name, border = "blue", col = "green")
    # Generate the Q-Q plot
    qqnorm(column_data, main = paste("Q-Q Plot of", column_name))
    qqline(column_data, col = "red")
  }
}

# Apply the function to the dataset to create plots
create_plots_for_numeric_columns(dataset)

# Rest of the code should be preserved as it is in the original file.




# x1, x2 and x3; x1 is normally distributed, x2 is slightly positively skewed/un-evenly distributed, x3 is strongly positively skewed/un-evenly distributed. 


# Load necessary libraries
library(ggplot2)
library(car)

# Comparing distributions of x1, x2, and x3
shapiro.test(dataset$x1)
shapiro.test(dataset$x2)
shapiro.test(dataset$x3)

# Code to check the distributions, and whether to reject the null hypothesis
hist(dataset$x1, main="Histogram of x1", xlab="x1")
hist(dataset$x2, main="Histogram of x2", xlab="x2")
hist(dataset$x3, main="Histogram of x3", xlab="x3")

# Checking for multicollinearity between x1 and x4
vif_result <- lm(y1 ~ x1 + x2 + x3 + x4, data=dataset)
vif_result

# VIF scores above 5 or 10 indicate high multicollinearity between IVs. Multicollinearity suggests that coefficients are not always reliable

# Testing Linearity of y1 with x1 and x3
plot(dataset$x1, dataset$y1, main="Scatter plot of x1 vs y1")

plot(dataset$x3, dataset$y1, main="Scatter plot of x3 vs y1")

cor(dataset$x1, dataset$y1)

cor(dataset$x3, dataset$y1)

# Testing Homoscedasticity in relationship between y1 with x1 and x2
model <- lm(y1 ~ x1 + x2, data=dataset)
plot(model, which = 1)


# 1. x1 = yes, x2 = no but small, x3 = no and very uneven
# 2. No it does not
# 3. x1 yes, x3 no
# 4.Yes Homoscedasticity is present as their is no pattern and the red line fits relatively close





