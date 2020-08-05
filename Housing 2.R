# Assignment: ASSIGNMEnt 1

# Name: Luma, Alberto

# Date: 15 March 2020

# Import the week-7-housing
week_7_housing<- read.delim(file.choose(), header=T, sep=",")
# Attach the data
attach(week_7_housing)

week_7_housing

# Explain why you choose to remove data points from your 'clean' dataset.
# Answer: data cleaning is the process of detecting and correcting (or removing) corrupt or inaccurate records from a record set, table,
          # or database and refers to identifying incomplete, incorrect, inaccurate or irrelevant parts of the data and then replacing, modifying, 
          # or deleting the dirty or coarse data.Data cleansing is also important because it improves your data quality and in doing so, increases overall productivity.

# Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) 
library(ggplot2)

ggplot(week_7_housing, aes(x = Sale.Price, y = square_feet_total_living)) + 
  geom_point() +
  labs(x = 'Sale.Price', y = 'square_feet_total_living', title = 'Relationship between Sale.Price and square_feet_total_living')

cov(Sale.Price, square_feet_total_living, method = "pearson")

cor(Sale.Price, square_feet_total_living, method = "pearson")

cor.test(Sale.Price, square_feet_total_living, method = "pearson", alt="greater", conf.level=0.99)

head(Sale.Price)
head(square_feet_total_living)

# And one that will contain Sale Price, Bedrooms, and Bath Full Count as predictors.
head(Sale.Price)
head(bedrooms)
head(bath_full_count)

install.packages("ppcor")
library(ppcor)

t.test(Sale.Price, bedrooms, mu=0, alt="two.sided", paired=T, conf.level=0.99)
cor.test(x = Sale.Price, y = bedrooms)
pcor.test(x = Sale.Price, y = bedrooms, z = bath_full_count)

# Execute a summary() function on two variables defined in the previous step to compare the model results.
week_7_housing1 <- lm(Sale.Price ~ bedrooms + bath_full_count)
summary(week_7_housing1)

# the Multiple R-squared=0.1023 and	Adjusted R-squared=0.1022  represent approximately 10% of variation in Sale.Price can be explained by our model (bedrooms + bath_full_count). 

# Calculate the confidence intervals for the parameters in your model and explain what the results indicate.

var(Sale.Price)
var(bedrooms)
var(Sale.Price)/var(bedrooms)
var.test(Sale.Price,bedrooms)

N<- length(Sale.Price)
N
a<- mean(Sale.Price)
a
b<- mean(bedrooms)
b
c<- mean(bath_full_count)
c

confint(week_7_housing1, conf.level=0.95)

# 95 percent confidence interval: since I have a 95% interval, and my means are at 3.48 for bedrooms, 1.8 for bathroom and 660737.7 for sale price,
# this indicates that my population mean is greater than 205795955089 but lower than 220524948226.

plot(week_7_housing1)

# calculate the standardized residuals
Residuals<- residuals(week_7_housing1)
predict(week_7_housing1)
plot(Sale.Price, Residuals)
plot(bedrooms, Residuals)
plot(bath_full_count, Residuals)

# The sum of large residuals.
sum(Residuals)
residuals(week_7_housing1)^2
sum(residuals(week_7_housing1)^2)

# Overall, is this regression model unbiased?
# Bias only occurs when the omitted variable is correlated with both the dependent variable and one of the included independent variables.
# Yes, this regression model is unbiased.

