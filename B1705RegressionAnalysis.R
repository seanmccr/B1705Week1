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

# Check for multicollinearity
# Load the car package, which lets us calculate VIF
library(car)

# Then, check for multicollinearity
vif(model)

# ----- 4. Multiple Linear Regression: Practice -----

df <- read.csv('https://www.dropbox.com/scl/fi/4eb55qzffd3cdnrpl5h0i/mreg_01.csv?rlkey=zddodcpd6df5xfwivkyhefett&dl=1')

model2 <- lm(score ~ wind_speed + pressure + equipment_quality + archer_experience_years, data = df)
summary(model2)


# Insert formatting for report shown in section 7 within week 1


# ----- 5. Regression Coefficients -----

# Load the dataset
data(mtcars)

# Fit the model
model <- lm(mpg ~ wt + hp, data = mtcars)

# Standardising coefficients hin this code here
# Extracting unstandardized coefficients
coefficients <- coef(model)
print(coefficients)

# Standardising the variables
mtcars$wt_std <- scale(mtcars$wt)
mtcars$hp_std <- scale(mtcars$hp)

# Fitting the model with standardised predictors
model_std <- lm(mpg ~ wt_std + hp_std, data = mtcars)

# Extracting standardized coefficients
coefficients_std <- coef(model_std)
print(coefficients_std)

# Viewing the summary of the model
summary(model)

# t-value and p-value scores < 0.05 suggests we reject the null hypothesis (that the coefficient is 0), indicating their is a significant relationship between variables



