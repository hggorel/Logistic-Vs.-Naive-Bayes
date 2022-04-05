# Homework 6

#Naive Bayes
install.packages("naivebayes")
library(naivebayes)

mydata <- read.csv("~/nba_logreg.csv", header=T, stringsAsFactors=T)
indexes <- sample(2, nrow(mydata), replace=T, prob=c(0.8,0.2))
train <- mydata[indexes==1, ]
test <- mydata[indexes==2, ]

head(train)

#Naive Bayes Tests

model <- naive_bayes(as.factor(TARGET_5Yrs) ~ .-Name, data=train, laplace=1, usekernel = T)
plot(model)

p <- predict(model, test)
table(p, test$TARGET_5Yrs)


# Logistic Regression

glm.fit <- glm (as.factor(TARGET_5Yrs)~GP+PTS+FG.+FT.+REB-Name, data = train, family = binomial)
#glm.fit <- glm (as.factor(TARGET_5Yrs)~. -Name, data = train, family = binomial)
summary(glm.fit)
plot(glm.fit)

glm.probs <- predict(glm.fit, test, type = "response")
# plot(glm.probs)
glm.probs[1:5]

glm.pred <- ifelse(glm.probs>0.5, "Yes", "No")
attach(test)
table(glm.pred, TARGET_5Yrs)
