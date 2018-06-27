# correlation ####
library(datasets)
data <- airquality
colMeans(is.na(data)) * 100
data_filtered <- filter(data, !is.na(Ozone) & !is.na(Solar.R))
cor(data_filtered)

# Simple Linear Regression ####
faithful$eruptions
faithful$waiting
?lm
erruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(erruption.lm)

coeffs <- erruption.lm$coefficients # model coefficients
eruption_df <- data.frame(waiting = faithful$waiting,
                          duration = coeffs[1] + coeffs[2] * faithful$waiting)
eruption_df %>%
  filter(waiting >= 75 & waiting <= 85) %>%
  arrange(waiting)

summary.lm <- summary(erruption.lm)
summary.lm$r.squared
#r squared (Coefficient of Determination) > Wsp????czynnik determinacji, inaczej zwany wsp????czynnikiem okre??lono??ci lub R-kwadrat jest miar?? tego, jaki procent zmienno??ci zmiennej zale??nej (obja??nianej) jest wyja??niany za pomoc?? zmiennej niezale??nej

summary(erruption.lm)
#p-value > As the p-value is much less than 0.05, we reject the null hypothesis. Hence there is a significant relationship between the variables in the linear regression model of the data set faithful.

# now, predict how much duration will last taking into account that we wait 123 minutes
new_data <- data.frame(waiting = 123)
?predict
predict(erruption.lm, new_data, interval = "confidence")

# residuals plot
df_res <- data.frame(waiting = faithful$waiting,
                     eruptions = faithful$eruptions,
                     res = erruption.lm$residuals)
with(df_res, plot(waiting, res))

# Multiple Linear Regression (MLR) ####
# predict the stack loss if the air flow is 72, water temperature is 20 and acid concentration is 85.
str(stackloss)
stackloss.lm <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
new_data <- data.frame(Air.Flow = 72,
                       Water.Temp = 20,
                       Acid.Conc. = 85)
predict(stackloss.lm, new_data) # result of prediction
predict(stackloss.lm, new_data, interval = "confidence") # result of prediction with confidence
summary.lm <- summary(stackloss.lm)
summary.lm$r.squared
summary.lm$coefficients

# Logistic Regression ####
# jedna z metod regresji u??ywanych w statystyce w przypadku, gdy zmienna zale??na jest na skali dychotomicznej (przyjmuje tylko dwie warto??ci). Zmienne niezale??ne w analizie regresji logistycznej mog?? przyjmowa?? charakter nominalny, porz??dkowy, przedzia??owy lub ilorazowy. 

# in mtcars DS,
# the data column am represents the transmission type of the automobile model 
# (0 = automatic, 1 = manual). 
# With the logistic regression equation, we can model the probability 
# of a manual transmission in a vehicle based on its 
# engine horsepower and weight data.

# By use of the logistic regression equation of vehicle transmission in the data set mtcars, estimate the probability of a vehicle being fitted with a manual transmission if it has a 120hp engine and weights 2800 lbs.
str(mtcars)
am.glm <- glm(am ~ hp + wt, data = mtcars)
summary(am.glm)
save(am.glm, file = "am_glm_model.rda")
# new data arrived
new_data <- data.frame(hp = 120,
                       wt = 2.8)
load("am_glm_model.rda")
predict(am.glm, newdata = new_data) # the probability of it being fitted with a manual transmission is about 53%.
#we can use "update" to update the model with new data

# Neural nets ####
library(neuralnet)
library(dplyr)
data <- read.csv(file = "synth.te.csv.csv")
str(data)
hist(data$xs)
hist(data$ys)
hist(data$yc)
?neuralnet

sample_size <- floor(0.7 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#train[, "yc"] <- factor(train[, "yc"])
model <- neuralnet(formula = yc ~ xs + ys, data = train, hidden = c(6,5))
plot(model)

result <- compute(model, test[, c("xs", "ys")])
prediction <- round(as.numeric(result$net.result))
test_set <- data.frame(test = test,
                       prediction = prediction)
test_set[, c("test.yc", "prediction")]
test_set[, "correct"] <- ifelse(test_set$test.yc == test_set$prediction, 1, 0)
nrow(test_set[test_set$correct == 1, ]) / nrow(test_set)

# Deep learning ####
#sort of....
library(nnet)
head(iris)
str(iris)
table(iris$Species)
sample_size <- floor(0.7 * nrow(iris))
index <- sample(seq_len(nrow(iris)), size = sample_size)
train <- iris[index, ]
test <- iris[-index, ]
?nnet
ir.nn <- nnet(Species ~ ., data = train, size = 6, rang = 0.1,
              decay = 1e-2, maxit = 2000)
plot(ir.nn)

labels.nnet <- predict(ir.nn, test, type = "class")
table(test$Species, labels.nnet)
mean(test$Species == labels.nnet)

# Decision trees ####
library(rpart)
str(iris)
sample_size <- floor(0.7 * nrow(iris))
tain_index <- sample(seq_len(nrow(iris)), size = sample_size)
train <- iris[index, ]
test <- iris[-index, ]

model <- rpart(Species ~ ., data = train, method = "class")
plot(model)
text(model, use.n = TRUE)
?rpart
?prune
prediction <- as.data.frame(round(predict(model, test[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])))
head(iris)
table(iris$Species)
prediction[, "prediction"] <- ifelse(prediction$setosa == 1, "setosa",
                                     ifelse(prediction$versicolor == 1, "versicolor",
                                            ifelse(prediction$virginica == 1, "virginica",
                                                   NA)))
test[, "prediction"] <- prediction$prediction
table(test$Species, test$prediction)
mean(test$Species == test$prediction)

# decision trees
fit <- with(data, rpart(Kyphosis ~ Age + Number + Start, method = "class"))
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit)
text(fit, all = TRUE)
plot(fit, uniform = TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n = TRUE, all = TRUE, cex = .8)

# Segmentation ####
# provide segments of data
str(airquality)
data <- airquality %>% dplyr::select(Ozone, Solar.R, Wind, Temp)

# normalize data
normalize <- function(data) {
  res <- (data - min(data, na.rm = TRUE)) / (max(data, na.rm = TRUE) - min(data, na.rm = TRUE))
  res <- round(res * 5)
  return(res)
}

for(i in 1:ncol(data)) {
  col_name <- names(data)[i]
  new_name <- paste0(col_name, "_norm")
  data[, new_name] <- normalize(data[, i])
}

#segmentation (k-means)
data <- data %>% dplyr::select(Ozone_norm, Solar.R_norm, Wind_norm, Temp_norm)
data[is.na(data)] <- 0 # remove NA's
?kmeans
model_km <- kmeans(data, centers = 5)
data[, "k_means"] <- model_km$cluster

# Hierarchical clustering ###
?dist
?hclust

data_2 <- data %>% dplyr::select(Ozone_norm, Solar.R_norm, Wind_norm, Temp_norm)
data_hc <- as.matrix(data_2) %>% dist %>% hclust
plot(data_hc)

data[, "hclust"] <- cutree(data_hc, 5)

# hierarchical clustering ####
library(dplyr)
set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1, 2, 1), each = 4), 0.2)

data <- data.frame(x = x, y = y)
dist(data) # distance matrix
hClustering <- data %>% dist %>% hclust
plot(hClustering)
str(hClustering)

hClustering$height
cutree(hClustering, k = 5) # number of clusters

# k-means ####
set.seed(9235)
x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1, 2, 1), each = 4), 0.2)

data <- data.frame(x = x, y = y)
plot(data)

?kmeans
clusters <- kmeans(data, centers = 4)
str(clusters)

clusters$cluster
with(data, plot(x, y, pch = 20, main = "Clusters", xlab = "X", ylab = "Y"))
points(clusters$centers, pch = 3, lwd = 3)

library(ggplot2)
data[, "cluster"] <- clusters$cluster
with(data, qplot(x, y, colour = cluster))

# Outlier detection ###

# Model validation ####

# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method = "cv", number = 10, repeats = 3)
# train the model
model <- train(Species ~ ., data = iris, trControl = train_control, method = "svmLinearWeights2")
?train
# summarize results
print(model)

# SVM ###
library("e1071")
?svm

# FINAL test ####
### IRIS
## feature selection - then the rest
library(Boruta)
library(dplyr)
library(nnet)
str(iris)
bor <- Boruta(Species ~., data = iris)
bor <- as.data.frame(bor$finalDecision)
bor[, "feature"] <- rownames(bor)
selected_features <- bor[bor$`bor$finalDecision` == "Confirmed", "feature"]
data <- iris[, c(selected_features, "Species")]

#train/test
sample_size <- floor(0.75 * nrow(iris))
index <- sample(seq_len(nrow(iris)), size = sample_size)
train <- data[index, ]
test <- data[-index, ]

fit_nn <- nnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, , data = train, size = 4)
prediction_nn <- as.data.frame(predict(fit_nn, test))
prediction_nn <- round(prediction_nn)

test[, "prediction_nn"] <- ifelse(prediction_nn$setosa == 1, "setosa", 
                                  ifelse(prediction_nn$versicolor == 1, "versicolor", 
                                         ifelse(prediction_nn$virginica == 1, "virginica", NA))) 
table(test$Species, test$prediction_nn)

### AIRQUALITY - viz and stuff
library(datasets)
library(help = "datasets")
str(airquality)
data <- airquality

# why we have incresed level of Ozone in the air ???
par(mfrow = c(2,2))
hist(data$Ozone)
hist(data$Solar.R)
hist(data$Wind)
hist(data$Temp)
par(mfrow = c(1,1))

colMeans(is.na(data)) # we have 24% of NA's for Ozone > remove them
data <- data %>% filter(!is.na(Ozone) & !is.na(Solar.R))

cor(data) # > Ozone correlates with Wind and Temp
par(mfrow = c(2, 1))
with(data, plot(Ozone, Wind))
with(data, plot(Ozone, Temp))
abline(with(data, lm(Ozone ~ Temp)))

par(mfrow = c(2, 1))
range(data$Wind)
wind_cut <- seq(2, 21, 1)
data[, "wind_cut"] <- cut(data$Wind, wind_cut)
with(data, boxplot(Ozone ~ wind_cut, main = "Ozone level by wind"))

range(data$Temp)
temp_cut <- seq(57, 97, 1)
data[, "temp_cut"] <- cut(data$Temp, temp_cut)
with(data, boxplot(Ozone ~ temp_cut, main = "Ozone level by temp"))
