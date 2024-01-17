# ----- B1705: Week One | Data Analysis and Interpretation - Practical | 17.01.2024 -----
 
# ----- 1. Preparation: Creating a sample vector -----
 
# Generate a sample vector
samplevector <- c(rep(10, 50), rep(20, 25), rep(30, 20), rep(40, 5), rep(3, 200), 1000)
# Code to provide sample summary
summary(samplevector)
# Code to provide the mean in a different format
themean <- mean(samplevector)
print(paste("Mean: ", themean))
# Code to provide the median in a different format
print(paste("Median:", median(samplevector)))
# Code to provide the mode in a different format
modefunc <- function(v) {
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}
  
print(paste("Mode: ", modefunc(samplevector)))
  
  
# Code to provide a visualisation of the differences between the mean and median/mode
# Function to calculate mode
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}
  
# Calculate mean, median, and mode
mean_value <- mean(samplevector)
median_value <- median(samplevector)
mode_value <- get_mode(samplevector)
  
# Create a plot
hist(samplevector, main = "Vector Display with Mean, Median, and Mode", 
  xlab = "Values", col = "lightgray", border = "gray")
  
# Add lines for mean, median, and mode
abline(v = mean_value, col = "red", lwd = 2)
abline(v = median_value, col = "green", lwd = 2)
abline(v = mode_value, col = "blue", lwd = 2)
  
# Add a legend
legend("topright", legend = c("Mean", "Median", "Mode"),
  col = c("red", "green", "blue"), lwd = 2)

# ----- 1.2. Measures of Variability -----
  
# Create a new sample vector
sample_vector <- c(rep(1, 50), rep(2, 25), rep(3, 20), rep(4, 5), 5)

# Method One - using the 'psych' package
# Loading library
library(psych)
# Summary code
summary_data <- describe(sample_vector)
print(summary_data)


# Method Two - using base R
# Calculate and print the range of the vector
range_value <- range(sample_vector)
print(paste("Range: [", range_value[1], ",", range_value[2], "]", sep=""))

# Calculate and print the standard deviation
print(paste("Standard Deviation:", round(sd(sample_vector), 2)))

# Calculate range and standard deviation
range_values <- range(sample_vector)
sd_value <- sd(sample_vector)

# Create a basic plot
plot(sample_vector, main = "Vector with Range and Standard Deviation", 
     xlab = "Index", ylab = "Values", pch = 19, col = "blue")
# Marking the range
abline(h = range_values[1], col = "red", lwd = 2) # lower range
abline(h = range_values[2], col = "red", lwd = 2) # upper range
# Marking the standard deviation
mean_value <- mean(sample_vector)
abline(h = mean_value + sd_value, col = "green", lwd = 2, lty = 2) # mean + SD
abline(h = mean_value - sd_value, col = "green", lwd = 2, lty = 2) # mean - SD
# Add a legend
legend("bottomright", legend = c("Lower Range", "Upper Range", "Mean ± SD"),
       col = c("red", "red", "green"), lwd = 2, lty = c(1, 1, 2))

# Creating a density plot with the mean
# Calculate the mean
mean_value <- mean(sample_vector)
# Create a density plot
plot(density(sample_vector), main = "Density Plot with Mean", 
     xlab = "Values", ylab = "Density", col = "blue", lwd = 2)
# Marking the mean
abline(v = mean_value, col = "red", lwd = 2)
# Add a legend
legend("topright", legend = c("Mean"),
       col = c("red"), lwd = 2)

##### 1.2.1. Negatively Skew Vector #####
# Code to load library
library(ggplot2)

# Negatively-skewed vector: More higher values and a tail on the left side
neg_skewed_vector <- c(rep(1, 5), rep(2, 20), rep(3, 25), rep(4, 50))

# Plot the negatively-skewed vector
ggplot(data.frame(value=neg_skewed_vector), aes(value)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Negatively-Skewed Vector", x="Value", y="Frequency")

##### 1.2.2. Positive Skew Vector #####
# Positively-skewed vector: More lower values and a tail on the right side.
pos_skewed_vector <- c(rep(1, 50), rep(2, 25), rep(3, 20), rep(4, 5))

# Plot the positively-skewed vector
ggplot(data.frame(value=pos_skewed_vector), aes(value)) +
  geom_histogram(binwidth=1, fill="red", color="black", alpha=0.7) +
  labs(title="Positively-Skewed Vector", x="Value", y="Frequency")

##### 1.2.3 Symmetrical Vector #####
# Symmetrical vector: Equal distribution on both sides of the central value.
symmetrical_vector <- c(rep(1, 18), rep(2, 24), rep(3, 30), rep(4, 25), rep(5,19))

# Plot the symmetrical vector
ggplot(data.frame(value=symmetrical_vector), aes(value)) +
  geom_histogram(binwidth=1, fill="green", color="black", alpha=0.7) +
  labs(title="Symmetrical Vector", x="Value", y="Frequency")

##### 1.2.4 Checking the skewness #####
# load the e1071 package
library(e1071)

# Calculate the skewness
skew_value <- skewness(sample_vector)

# Print and describe the skewness as output in the console
print(paste("Skewness:", round(skew_value, 2)))

if (skew_value > 0) {
  print("The distribution is positively skewed (right-tailed).")
} else if (skew_value < 0) {
  print("The distribution is negatively skewed (left-tailed).")
} else {
  print("The distribution is approximately symmetric.")
}

##### 1.2.5 Plotting Vector Distribution #####
# Code to create plot 
ggplot(data.frame(value=sample_vector), aes(value)) +
  geom_histogram(binwidth=1, fill="green", color="black", alpha=0.7) +
  labs(title="Sample Vector - Positively Skewed with more values to the left", x="Value", y="Frequency")

# Use Stem and leaf plot to visualise range
#| code-fold: true
#| code-summary: Show code for stem-and-leaf plot

# Create a stem-and-leaf plot
stem(pos_skewed_vector)


##### 1.2.6 Confidence Intervals #####
# We create a function to calculate the confidence interval of a numeric vector

calculate_confidence_interval <- function(data, confidence_level = 0.95) {
  # Check if the data is a numeric vector
  if (!is.numeric(data)) {
    stop("Data must be a numeric vector.")
  }
  
  # Use t.test to calculate the confidence interval
  test_result <- t.test(data, conf.level = confidence_level)
  
  # Extract the confidence interval
  ci <- test_result$conf.int
  
  # Return the confidence interval
  return(ci)
}

# Example usage
data_vector <- c(12, 15, 14, 16, 15, 14, 16, 15, 14, 15)
ci <- calculate_confidence_interval(data_vector)
print(ci)


# ----- 1.3. Visualisation -----
##### 1.3.1. Creating Bar Charts and Histograms for categorical and continuous data #####
# We create some categorical data
teams <- c("Liverpool", "Man City", "Chelsea", "Brighton", "West Ham")
goals_for <- c(34, 45, 21, 15, 10)

# And some continuous data
goals <- rnorm(200, mean=30, sd=6)  # Simulated goals

# Bar chart for categorical data
bar_chart <- ggplot(data.frame(teams=teams, goals_for=goals_for), aes(x=teams, y=goals_for)) +
  geom_bar(stat="identity", fill="coral", color="black", width=0.7) +
  labs(title="Bar Chart for Categorical Data (goals for)", x="Team", y="Goals For") +
  theme_minimal()

print(bar_chart)

# Histogram for the continuous data set
histogram_plot <- ggplot(data.frame(goals=goals), aes(goals)) +
  geom_histogram(binwidth=5, fill="skyblue", color="black", alpha=0.7) +
  labs(title="Histogram for Continuous Data", x="Goals", y="Frequency") +
  theme_minimal()

print(histogram_plot)

##### 1.3.2. Creating Boxplot of Dummy Dataset #####
# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate dummy dataset
dummy_data <- data.frame(
  player_id = sample(c("Player 1", "Player 2", "Player 3"), 1000, replace = TRUE),
  distance_covered = runif(1000, min = 1, max = 100)
)

# View the first few rows of the dataset
head(dummy_data)

# Create a boxplot
boxplot <- ggplot(dummy_data, aes(x = player_id, y = distance_covered)) +
  geom_boxplot() +
  labs(title = "Boxplot of Distance Covered Grouped by Player",
       x = "Player",
       y = "Distance (m)")

# Print the boxplot
print(boxplot)

##### 1.3.3. Creating Pie Charts #####
library(ggplot2)

# Calculate proportions for each category level
category_counts <- dummy_data %>%
  group_by(player_id) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Create a pie chart using the players
pie_chart <- ggplot(category_counts, aes(x = "", y = Proportion, fill = player_id)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Category Levels",
       fill = "Category")

# Print the pie chart
print(pie_chart)


####### COMPLETE 9.3 FROM WEEK 1 TASKS AT HOME #######







