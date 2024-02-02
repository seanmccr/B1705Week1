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


# ----- 1.4. Challenges -----

##### 1.4.1. Dealing with Missing Data #####
# Set seed for reproducibility
set.seed(123)
# Generate a vector with 50 random observations from a normal distribution
observations <- rnorm(50, mean = 10, sd = 5)
# Randomly assign missing values to 10% of the observations
missing_indices <- sample(1:50, 5)
observations[missing_indices] <- NA
# Print the vector with missing values
print(observations)


# Removing observations with missing values
clean_observations <- observations[!is.na(observations)]
# Printing the cleaned observations
print(clean_observations)

# Doing so removes a lot of observations, which we don't necessarily want

# We can imputing missing values with a specific value, for example, 0
imputed_observations_01 <- ifelse(is.na(observations), 0, observations)
# Printing the imputed observations
print(imputed_observations_01)

# Often, we might want to use the mean of the vector.
# Calculating the mean of the non-missing values
mean_value <- mean(observations, na.rm = TRUE)
print(mean_value)

# Imputing missing values with the calculated mean
imputed_observations_02 <- ifelse(is.na(observations), mean_value, observations)
# Printing the imputed observations
print(imputed_observations_02)

# By putting the mean into the missing values, we can make data more reliable without removing missing values

# This time, we will remove NA values in either variable
# Creating a dataframe with two variables containing missing values
data <- data.frame(
  variable1 = c(1, NA, 3, 4, NA, 6),
  variable2 = c(NA, 2, NA, 4, 5, NA)
)
# Printing the original dataframe
print("Original Dataframe:")
# Removing observations with a missing value in EITHER variable
clean_data <- na.omit(data)
# Printing the cleaned dataframe
print("Cleaned Dataframe:")
print(clean_data)

# Here, we will put the mean of each column into the missing value entries
# Loading the dplyr package for easier data manipulation
library(dplyr)
# Creating a dataframe with two variables containing missing values
data <- data.frame(
  variable1 = c(1, NA, 3, 4, NA, 6),
  variable2 = c(NA, 2, NA, 4, 5, NA)
)
# Printing the original dataframe
print("Original Dataframe:")
print(data)

# Function to replace NA with the mean of the column
replace_na_with_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}
# Applying the function to each column using `across`
imputed_data <- data %>% mutate(across(everything(), replace_na_with_mean))
# Printing the dataframe with imputed values
print("Dataframe with Imputed Values:")
print(imputed_data)

# By doing this, we get imputed values at each stage, with the mean updated every time their is a new value imputed.

##### 1.4.2. Managing Outliers #####

# Remove outliers based on Z-scores
# Generate a synthetic dataset
set.seed(123) # for reproducibility
data <- data.frame(value = rnorm(100, mean = 50, sd = 10)) # synthetic normal data
# Function to calculate Z-scores
calculate_z_score <- function(x) {
  (x - mean(x)) / sd(x)
}
# Apply the function to calculate Z-scores for the dataset
data$z_score <- calculate_z_score(data$value)
# Define threshold for outliers
lower_bound <- -3
upper_bound <- 3
# Remove outliers
cleaned_data <- data[data$z_score > lower_bound & data$z_score < upper_bound, ]
# View the cleaned data
print(cleaned_data)

# This approach again removes values, and is therefore problematic; the approach below imputes values based on z-score if they are missing
# Generate a synthetic dataset
set.seed(123) # for reproducibility
data <- data.frame(value = rnorm(100, mean = 50, sd = 10)) # synthetic normal data
# Function to calculate Z-scores
calculate_z_score <- function(x) {
  (x - mean(x)) / sd(x)
}
# Apply the function to calculate Z-scores for the dataset
data$z_score <- calculate_z_score(data$value)
# Define threshold for outliers
lower_bound <- -3
upper_bound <- 3
# Impute outliers using the mean
data$value[data$z_score < lower_bound] <- mean(data$value) - 3 * sd(data$value)
data$value[data$z_score > upper_bound] <- mean(data$value) + 3 * sd(data$value)
# Remove the z_score column if no longer needed
data$z_score <- NULL
# View the modified data
print(data)

# ----- 2. Data Analysis: Practice -----
# Download dataset
url <- "https://www.dropbox.com/scl/fi/fmqfeq6ivdrvnr4hy79ny/prac_14_dataset.csv?rlkey=spz5e5uq1b8if2eptvstxo7xj&dl=1"
df <- read.csv(url)
rm(url)

# Boxplots
boxplot(df)

# CALCULATING THE MEASURES OF CENTRAL TENDANCY
# Calculate the mean, median, and mode for each suitable variable in the dataframe 'df_mean_imputed'
# Step 2: Create another new dataframe with no missing values by imputing missing values using the column mean
df_mean_imputed <- df
summary_stats <- data.frame(Variable=character(), Mean=numeric(), Median=numeric(), Mode=numeric(), stringsAsFactors=FALSE)

for(column in names(df_mean_imputed)){
  if(is.numeric(df_mean_imputed[[column]])) {
    # Calculate mean, median, and mode
    column_mean <- mean(df_mean_imputed[[column]], na.rm = TRUE)
    column_median <- median(df_mean_imputed[[column]], na.rm = TRUE)
    column_mode <- mode(df_mean_imputed[[column]])
    
    # Append the values to the summary_stats dataframe
    summary_stats <- rbind(summary_stats, data.frame(Variable=column, Mean=column_mean, Median=column_median, Mode=column_mode))
  }
}
# Print the summary statistics for all numerical variables
print(summary_stats)




# CALCULATING THE MEASURES OF VARIABILITY
# Create a data frame to store the range and standard deviation values for each variable
variable_stats <- data.frame(Variable=character(), Range=numeric(), SD=numeric(), stringsAsFactors=FALSE)

for (column in names(df_mean_imputed)) {
  if (is.numeric(df_mean_imputed[[column]])) {
    # Calculate the range
    column_range <- range(df_mean_imputed[[column]], na.rm = TRUE)
    # Calculate the standard deviation
    column_sd <- sd(df_mean_imputed[[column]], na.rm = TRUE)
    # Append the results to the variable_stats data frame
    variable_stats <- rbind(variable_stats, 
                            data.frame(Variable=column, Range=diff(column_range), SD=column_sd))
  }
}

# View the results
print(variable_stats)


# MEASURES OF DISTRIBUTION

# The variable with the negative skew is 

ggplot(data.frame(value=df_mean_imputed), aes(x = Var3)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Negatively-Skewed Vector", x="Value", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var1)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var2)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var3)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var5)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var6)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var7)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")

ggplot(df_mean_imputed, aes(x=Var8)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram for Var1", x="Var1", y="Frequency")


