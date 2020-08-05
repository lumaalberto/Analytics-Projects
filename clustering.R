# Assignment: ASSIGNMEnt 2

# Name: Luma, Alberto

# Date: 29 March 2020

# Import and create a dataframe with Dodgers Data
dodgers<- read.delim(file.choose(), header=T, sep=",")
# Attach the data

attach(dodgers)
dodgers

# Import libraries
library(lattice)  # Graphics Package
library(ggplot2) # Graphical Package

# Check the structure of the data
str(dodgers)

# Rearrange the day_of_week (cleaning data)
dodgers$day_of_week <- factor(dodgers$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Rearrange the month (cleaning data)
dodgers$month <- factor(dodgers$month, levels = c("APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT"))

head(dodgers, 10)

# Boxplot for attendance by day of week
plot(dodgers$day_of_week, dodgers$attend / 1000, main = "Dodgers Attendence By Day Of Week", xlab = "Day of Week", ylab = "Attendance (thousands)", col = "green", las = 1)

# Boxplot for attendance by Month
plot(dodgers$month, dodgers$attend / 1000, main = "Dodgers Attendence By Month", xlab = "Month", 
     ylab = "Attendance (thousands)", col = "light green", las = 1)

#Scatterplot for attendance by weather
ggplot(dodgers, aes(x=temp, y=attend/1000, color=fireworks)) + 
  geom_point() + 
  facet_wrap(day_night~skies) + 
  ggtitle("Dodgers Attendance By Temperature By Time of Game and Skies") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="light blue", size=10)) +
  xlab("Temperature") +
  ylab("Attendance to the Thousands")

#ScatterPlot of Attendance by opponent
ggplot(dodgers, aes(x=attend/1000, y=opponent, color=day_night)) + 
  geom_point() + 
  ggtitle("Dodgers Attendance By Opponent") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="light blue", size=10)) +
  xlab("Attendance to the Thousand") +
  ylab("Opponent")


# Model with the bobblehead variable
alberto.model <- {attend ~ month + day_of_week + bobblehead}
alberto.model

#Training and Test dataset

# Reseed for repeatability
set.seed(1234)

training_test <- c(rep(1, trunc((2/3)*nrow(dodgers))), rep(2, trunc((1/3)*nrow(dodgers))))
training_test

# Create a variable in DodgersData data frame to identify Test and Training row
dodgers$Training_Test <- sample(training_test)
dodgers$Training_Test

dodgers$Training_Test <- factor(dodgers$Training_Test, levels = c(1, 2), labels = c("TRAIN", "TEST"))

dodgers.Train <- subset(dodgers, Training_Test == "TRAIN")
dodgers.Test <- subset(dodgers, Training_Test == "TEST")

# Fit model to training set
train.model.fit <- lm(alberto.model, data = dodgers.Train)
train.model.fit

# Predict from Training Set
dodgers.Train$Predict_Attend <- predict(train.model.fit)
dodgers.Train$Predict_Attend

# Fitted Model on the Test Set
dodgers.Test$Predict_Attend <- predict(train.model.fit, newdata = dodgers.Test)
dodgers.Test$Predict_Attend

# Attendance due to bobbleheads
alberto.model.fit <- lm(alberto.model, data = dodgers) 
print(summary(alberto.model.fit))

cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ", round(alberto.model.fit$coefficients[length(alberto.model.fit$coefficients)], digits = 0),"\n",sep="")

