# graphics ####
# q: Are there any counties in the U.S. that exceed the national standard for ozone?
# ???annual fourth-highest daily maximum 8-hr concentration, averaged over 3 years??? should not exceed 0.075 parts per million (ppm)
# ???annual highest daily max??? should not exceed 0.11 parts per million (ppm)

# first - prepare data
str(ozone)
as.Date(ozone$Date.Local)
result <- ozone %>%
  select(State.Name, County.Name, Date.Local, Sample.Measurement) %>%
  mutate(Date.Local = as.Date(Date.Local)) %>%
  group_by(State.Name, County.Name, Date.Local) %>%
  summarize(ozone_max = max(Sample.Measurement, na.rm = TRUE))

result <- result %>%
  group_by(State.Name, County.Name) %>%
  summarize(ozone_max = max(ozone_max, na.rm = TRUE))

# next - plot it
summary(result$ozone_max)

boxplot(result$ozone_max, col = "blue")
abline(h = 0.11)

hist(result$ozone_max, breaks = 100, col = "green")
abline(v = 0.11, col = "magenta", lwd = 4)

boxplot(result$ozone_max ~ result$State.Name)
abline(h = 0.11)

# base graphics ####
library(datasets)

data <- airquality
str(data)
getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/R_cources/Data_mining_course/data")
write.csv(data, "for_tableau.csv")

hist(data$Ozone)

# the same
data <- mutate(data, Month = factor(Month)) ## I prefer mutate as more generic
data <- transform(data, Month = factor(Month)) ## to use it in pipeline %>% you need to refrence to it by $

?boxplot
with(data, boxplot(Temp ~ Month, col = "green", xlab = "Month", ylab = "Temperature"))
abline(h = 75, lwd = 2, col = "red")

# scatterplot
with(data , plot(Temp, Ozone, type = "p"))
with(filter(data, Month == 5), points(Temp, Ozone, type = "p", col = "magenta"))
with(filter(data, Month != 5), points(Temp, Ozone, type = "p", col = "green"))
title("Ozone vs Temp")
legend("topright", pch = 1, col = c("magenta", "green"), legend = c("May", "Other months"))

# scatterplot with simple linear model (Ozone to Wind)
with(data, plot(Wind, Ozone, pch = 20))
model <- with(data, lm(Ozone ~ Wind))
abline(model, lwd = 2, col = "red")

# plotting multiple charts
par(mfrow = c(1, 2))
with(data, {
  boxplot(Wind ~ Month, col = "green", main = "boxplot", xlab = "Month", ylab = "")
  plot(Wind, Ozone, main = "")
  points(filter(data, Month == 5), col = "green")
  legend("topright", pch = 1, col = c("green", "black"), legend = c("May", "Other months"))
})

# plot with smoothing
str(airquality)
with(airquality, {
  plot(Temp, Ozone)
  lines(loess.smooth(Temp, Ozone))
})

# ggplot2 ####
library(ggplot2)
str(mpg)

?qplot
qplot(displ, hwy, data = mpg) # scatter plot
qplot(displ, hwy, data = mpg, color = "red") # scatter plot + simple color
qplot(displ, hwy, data = mpg, color = drv) # scatter plot + color by "drv"
qplot(displ, hwy, data = mpg, shape = drv) # scatter plot + shape by "drv"
qplot(displ, hwy, data = mpg, geom = c("smooth")) # plot only smooth line
qplot(displ, hwy, data = mpg, geom = c("point", "smooth")) # plot smooth lime and original points
qplot(hwy, data = mpg, geom_boxplot()) # simple boxplot
qplot(hwy, data = mpg, geom = "density") # simple density
qplot(hwy, data = mpg, fill = drv) # simple hist + color by "drv"
qplot(hwy, data = mpg, color = drv, geom = "density") # simple density + color by "drv"
qplot(displ, hwy, data = mpg, facets = .~ drv) # scatter plot + facet by drv
qplot(hwy, data = mpg, facets = drv ~.) # histogram + facet by drv (horizontal) 
qplot(hwy, data = mpg, facets = drv ~., fill = class) # histogram + facet by drv (horizontal) + fill by "class"
qplot(hwy, data = mpg, facets = drv ~., fill = class, binwidth = 1) # histogram + facet by drv (horizontal) + fill by "class" + binwidth

?ggplot
ggplot(mpg, aes(displ, hwy)) + geom_point() # scatter plot
ggplot(mpg, aes(displ, hwy, color = drv)) + geom_point() # scatter plot + simple color
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = "lm") # poits with lm model
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_grid(.~drv) + geom_smooth(method = "lm") # as above + facet


### Przypomnienie wiadomo??ci
library(datasets)
library(ggplot2)
airquality

str(airquality)
nrow(airquality)
ncol(airquality)
sapply(airquality, function(x) sum(is.na(x)))
sapply(airquality, function(x) length(unique(x)))

# histogram
hist(airquality$Ozone)
airquality <- mutate(airquality, mth = as.character(Month))
qplot(data = airquality, Ozone, color = mth, geom = "histogram", bins = 50, xlab = "xxx")
ggplot(airquality, aes(Ozone)) + 
  geom_histogram() +
  xlab("xxx") +
  ggtitle("main")
  
# density
plot(density(airquality[!is.na(airquality$Ozone), "Ozone"]))
qplot(data = airquality, Ozone, geom = "density")
ggplot(airquality, aes(x = Ozone, color = mth)) +
  geom_density()
ggplot(airquality, aes(x = Ozone, color = mth)) +
  geom_density() +
  facet_grid(mth~.)

# boxplot
with(airquality, boxplot(Ozone ~ mth))
ggplot(airquality, aes(x = mth, y = Ozone)) + 
  geom_boxplot()

# scatterplot
with(airquality, plot(Ozone, Wind))
with(filter(airquality, mth == "5"), points(Ozone, Wind, col = "green"))
abline(lm(airquality$Wind ~ airquality$Ozone))

qplot(data = airquality, Ozone, Wind, color = mth)

ggplot(airquality, aes(Ozone, Wind)) +
  geom_point() +
  geom_vline(xintercept = 40)

ggplot(airquality, aes(Ozone, Wind)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(airquality, aes(Ozone, Wind)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(mth~.)

# barchart
ggplot(airquality, aes(mth)) +
  geom_bar()
  






