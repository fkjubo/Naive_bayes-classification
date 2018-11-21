# importing data

data <- read.csv("binary.csv", T, ",")

# analysing data

str(data)

# changing structure of the data

data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)
data$gre <- as.numeric(data$gre)

# it is important for naive bayes that the independent variable has less correlation

library(psych)

pairs.panels(data[-1])

# analysing data through visulization

library(ggplot2)

ggplot(data, aes(x= admit, y= gre, fill= admit)) +
  geom_boxplot()

ggplot(data, aes(x= admit, y= gpa, fill= admit)) +
  geom_boxplot()

ggplot(data, aes(x= gre, fill= admit)) +
  geom_density(alpha= .5)

ggplot(data, aes(x= gpa, fill= admit)) +
  geom_density(alpha= .5)

# spiltting data into train and test

library(caTools)

set.seed(256)

split <- sample.split(data$admit, SplitRatio = .8)

train <- subset(data, split == T)
test <- subset(data, split == F)

# creating naive bayes model

library(caret)
library(naivebayes)

model <- naive_bayes(admit ~ .,
               data = train,
               usekernel = T)
               
plot(model)

# accuracy on the train data

pTrain <- predict(model, newdata = train)
confusionMatrix(pTrain, train$admit)

# accuracy on the test data

prediction <- predict(model, newdata = test)
confusionMatrix(prediction, test$admit)


