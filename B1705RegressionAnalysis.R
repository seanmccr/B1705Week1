# ----- B1705: Week One | Regression Analysis - Practical | 17.01.2024 -----

# ----- 1. Simple Linear Regression -----

# Load the dataset
data(mtcars)
# Load ggplot2
library(ggplot2)

# Create a scatter plot of mpg vs wt
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  ggtitle("Scatter plot of MPG vs Car Weight") +
  xlab("Car Weight") +
  ylab("Miles per Gallon")

# Fit a linear model
model <- lm(mpg ~ wt, data = mtcars)
# View the model summary
summary(model)

# Scatter Plot to show MPG vs Weight with regression line
# Load library
library(ggplot2)

# Using the mtcars dataset
data(mtcars)

# Create a scatter plot of mpg vs wt with added regression line
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Scatter Plot of MPG vs Car Weight with Regression Line") +
  xlab("Car Weight (1000 lbs)") +
  ylab("Miles per Gallon")


# ----- 2. Simple Linear Regression Task -----

##### 2.1. Creating a plot of Goals vs Assists #####
df <- read.csv('https://www.dropbox.com/scl/fi/28mws7intbsetmkl7y885/17_01_dataset.csv?rlkey=en9hg7xbwcl169v8418qz48h2&dl=1')

summary(df)
##### 2.1.1 Creating the scatter plot #####
ggplot(df, aes(x = Goals, y = Assists)) + 
  geom_point() +
  ggtitle("Scatter plot of Goals vs Assists") +
  xlab("Assists") +
  ylab("Goals")

##### 2.1.2 Checking assumption using simple linear regression #####
model <- lm(Goals ~ Assists, data = df)
# View the model summary
summary(model)

##### 2.1.3 Creating a plot for the relationship of these variables with a regression line #####
ggplot(df, aes(x = Goals, y = Assists)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Scatter Plot of Goals vs Assists with Regression Line") +
  xlab("Assists") +
  ylab("Goals")

# ----- 3. Multiple Linear Regression: Demonstration -----
# Load the dataset
data(mtcars)
# Fit a multiple linear regression model
model <- lm(mpg ~ wt + hp + qsec, data = mtcars)

# View the model summary
summary(model)


###### COMPLETE 


