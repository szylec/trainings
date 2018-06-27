setwd("/Users/slechwar/Google_Drive/04_Dev/R_cources/DM_case_studies")
rm(list = ls())

library(datasets)
library(dplyr)
str(iris)
str(airquality)

quantile(airquality$Ozone, na.rm = TRUE)
summary(airquality)
cor(airquality)

airquality %>% 
  group_by(Month) %>%
  summarize(Ozone_med = median(Ozone, na.rm = TRUE)) %>%
  mutate(Ozone_med_x2 = Ozone_med * 2) %>%
  arrange(desc(Ozone_med_x2))

with(iris, boxplot(Sepal.Length ~ Species))
with(iris, plot(Sepal.Length, Sepal.Width, col = Species))

library(MASS)
#remember this one
with(iris, parcoord(iris[, 1:4], col = Species))

# training and testing dataset
index <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train <- iris[index == 1, ]
test <- iris[index == 2, ]

# descission trees ####
index <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3)) 
train <- iris[index == 1, ]
test <- iris[index == 2, ]

library(rpart)
rpart <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train)
prediction <- as.data.frame(round(predict(rpart, test)))

prediction[, "result"] <- ifelse(prediction$setosa == 1, "setosa",
       ifelse(prediction$versicolor == 1, "versicolor", 
              ifelse(prediction$virginica == 1, "virginica", NA)))
table(prediction$result, test$Species)

# random forest ####
library(randomForest)
rf <- randomForest(Species ~ ., data = train, ntree = 100, proximity = TRUE)
plot(rf)
prediction_rf <- predict(rf, test)
table(prediction_rf, test$Species)

# linear regression ####
year <- rep(2008:2010, each = 4)
quarter <- seq(1, 4, 1)
cpi <- c(162.2, 164.6, 166.5, 166.0,
         166.2, 167.0, 168.6, 169.5,
         171.0, 172.1, 173.3, 174.0)
yq <- paste(year, quarter, sep = "Q")

data <- data.frame(y = as.numeric(year),
                   q = quarter,
                   yq = as.character(yq), 
                   cpi = as.numeric(cpi))
with(data, plot(yq, cpi))
cor(data$y, data$cpi)

fit <- lm(cpi ~ y + q, data = data)
fit_line <- fit$coefficients[1] + fit$coefficients[2] * data$y + fit$coefficients[3] * data$q
lines(fit_line, col = "red", lwd = 3)
summary(fit)
plot(fit)
fit$residuals

# logistic regression ####
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
rm(list = ls())
#setwd("/Users/slechwar/Google_Drive/04_Dev/R_cources/DM_case_studies/titanic_data_set")
setwd("/usr/local/google/home/slechwar/Google Drive/04_Dev/R_cources/DM_case_studies/titanic_data_set")
list.files()

train.raw <- read.csv("train.csv", na.strings = c(""), header = TRUE)
test.raw <- read.csv("test.csv", na.strings = c(""), header = TRUE)
gender.submission_raw <- read.csv("gender_submission.csv", na.strings = c(""), header = TRUE)

sapply(train.raw, function(x) sum(is.na(x))) # check for NAs
colMeans(is.na(train_raw)) * 100
sapply(train.raw, function(x) length(unique(x))) # check for unique values

# drop PassengerId and Cabin (too many NAs)
head(train.raw)
train.raw.sel <- train.raw %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
# change Age NAs with age mean 
train.raw.sel[is.na(train.raw.sel$Age), "Age"] <- mean(train.raw.sel$Age, na.rm = TRUE)
# remove Embarked NAs records from analysis
data <- train.raw.sel[!is.na(train.raw.sel$Embarked), ]
rownames(train.raw.sel) <- NULL
# split train set to two, original test set will be used for final validation
index <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[index == 1, ]
test <- data[index == 2, ]

model <- glm(data = train, formula = Survived ~., family = binomial(link = "logit"))
#summary(model)
#anova(model, test = "Chisq")

# check the model over new samples
fitted.results <- predict(model, newdata = subset(test, select = 2:ncol(test)), type='response')
fitted.results <- ifelse(fitted.results >= 0.5, 1, 0)
accuracy <- 1 - mean(fitted.results != test$Survived)

# 






