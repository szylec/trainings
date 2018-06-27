rm(list = ls())
setwd("/Users/slechwar/Google_Drive/04_Dev/tmp")
setwd("/Users/slechwar/Google_Drive/04_Dev/tmp/codewise")
library(dplyr)

# Statistics ####
# Qualiative data
library(MASS)
str(painters)
school <- painters$School
school.freq <- table(school)
school.freq <- cbind(school.freq) #frequencies
school.relfreq <- school.freq / nrow(painters) #percentage in all rows

old <- options(digits = 1) #change number of digits shown on the screen 
cbind(school.relfreq)
options(old) #restore old options
barplot(school.freq) #barplot
pie(school.freq) #pie chart

mean(painters$Composition, na.rm = TRUE)

painters %>%
  group_by(School) %>%
  summarise(Composition_mean = mean(Composition, na.rm = TRUE)) %>%
  arrange(desc(Composition_mean))

mean(painters[painters$School == "C", ]$Composition) #mean of only "C" School

# Quantitative data
str(faithful)
duration <- faithful$eruptions
range(duration)
breaks <- seq(1.5, 5.5, 0.5)
duration.cut <- cut(duration, breaks, right = FALSE) #convert duration to factor
duration.freq <- table(duration.cut)
hist(duration, breaks = 20, main = "Eruptions hist", xlab = "Duration, in minutes")

duration.freq_rel <- duration.freq / nrow(faithful) #relative freq
cbind(duration.freq_rel, duration.freq)
duration.cumsum <- cumsum(duration.freq)
cbind(duration.cumsum, duration.freq)

with(faithful, plot(eruptions, waiting))
abline(lm(faithful$waiting ~ faithful$eruptions))

# Numerical measures
duration <- faithful$eruptions
quantile(duration, c(0, 0.25, 0.5, 0.75, 1))
boxplot(duration, horizontal = TRUE)
sd(duration)
var(duration)
sd^2
# Kowariancja jest nieunormowan?? miar?? zale??no??ci liniowej pomi??dzy dwiema zmiennymi. Stanowi ona miar?? wsp??lnej zmienno??ci obu zmiennych (ko-wariancja) pomi??dzy zmiennymi. 
# Innymi s??owy czy odchylanie si?? obserwowanych wynik??w zmiennej od warto??ci ??redniej dla tej zmiennej jest podobne dla obu zmiennych. Je??eli
# zmienne nie s?? ze sob?? zwi??zane to kowariancja jest bliska warto??ci 0. Je??eli zmienne s?? ze sob?? zwi??zane to warto???? kowariancji jest r????na od 0. 
cov(duration, faithful$waiting)
#The covariance of eruption duration and waiting time is about 14. It indicates a positive linear relationship between the two variables.
cor(duration, faithful$waiting)
range(duration)
median(duration)
library(e1071)
skewness(duration)
kurtosis(duration)

# Probability distributions
#binormal
#Suppose there are twelve multiple choice questions in an English class quiz. Each question has five possible answers, and only one of them is correct. Find the probability of having four or less correct answers if a student attempts to answer every question at random.
?dbinom
dbinom(4, size = 12, prob = 0.2)

#random numbres generator
runif(3, min = 1, max = 10)
rnorm(3, mean = 10, sd = 2)

#normal distribution
#Assume that the test scores of a college entrance exam fits a normal distribution. Furthermore, the mean test score is 72, and the standard deviation is 15.2. What is the percentage of students scoring 84 or more in the exam?
?pnorm
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)

#chi squared
#sum?? k kwadrat??w niezale??nych zmiennych losowych o standardowym rozk??adzie normalnym
#Liczb?? stopni swobody mo??na uto??samia?? z liczb?? niezale??nych zmiennych losowych, kt??re wp??ywaj?? na wynik
#Find the 95th percentile of the Chi-Squared distribution with 7 degrees of freedom.
?qchisq
qchisq(0.95, df = 7)

#t-student
#Assume that a random variable Z has the standard normal distribution, and another random variable V has the Chi-Squared distribution with m degrees of freedom. Assume further that Z and V are independent, then the following quantity follows a Student t distribution with m degrees of freedom.
#Find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.
qt(c(0.025, 0.975), df = 5)

#f distribution
#If V 1 and V 2 are two independent random variables having the Chi-Squared distribution with m1 and m2 degrees of freedom respectively, then the following quantity follows an F distribution with m1 numerator degrees of freedom and m2 denominator degrees of freedom, i.e., (m1,m2) degrees of freedom.
#Find the 95th percentile of the F distribution with (5, 2) degrees of freedom.
qf(0.95, df1 = 5, df2 = 2)

# Interval estimation
library(MASS)
head(survey)
mean(survey$Height, na.rm = TRUE)

#Assume the population standard deviation ?? of the student height in survey is 9.48. Find the margin of error and interval estimate at 95% confidence level.

#desity and normality test - shapiro.wilk
height <- na.omit(survey$Height)
plot(density(height))
?shapiro.test
shapiro.test(survey$Height)

shapiro.test(runif(100, min = 2, max = 4))
plot(density(runif(100, min = 2, max = 4)))
shapiro.test(rnorm(5000, mean = 5, sd = 3))
plot(density(rnorm(100, mean = 5, sd = 3)))

qqnorm(height);qqline(height, col = 2)

n <- length(height)
sigma <- 9.48
sem <- sigma / sqrt(n) # standard error of the mean
E <- qnorm(0.975) * sem # margin of error
mean(height) + c(-E, E)

###
#...
###

# Hypothesis testing ####
#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. Assume the population standard deviation is 120 hours. At .05 significance level, can we reject the claim by the manufacturer?
n <- 30
xbar <- 9900
mu0 <- 10000
sd <- 120
sem <- sd / sqrt(n)
z <- (xbar - mu0) / sem
pval <- pnorm(z)

#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the sample standard deviation is 0.3 gram. At .05 significance level, can we reject the claim on food label?
xbar <- 2.1 # statystyka obliczona z pr??by
mu0 <- 2 # hipotetyczna warto???? parametru
sd <- 0.25 # odchylenie std
n <- 35 # ilos?? pr??bek
sem <- sd / sqrt(n) # standard error mean
z <- (xbar - mu0) / sem
pval <- pnorm(z, lower.tail = FALSE)  

# b????d Igo rodzaju (false positive)
#b????d polegaj??cy na odrzuceniu hipotezy zerowej, kt??ra w rzeczywisto??ci nie jest fa??szywa

# b????d IIgo rodzaju (false negative)
#b????d polegaj??cy na nieodrzuceniu hipotezy zerowej, kt??ra jest w rzeczywisto??ci fa??szywa

#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. Assume the actual mean amount of saturated fat per cookie is 2.09 grams, and the population standard deviation is 0.25 grams. At .05 significance level, what is the probability of having type II error for a sample size of 35 cookies?
mu0 <- 2
xbar <- 2.09
sd <- 0.25
n <- 35
sem <- sd / sqrt(n)

qnorm(0.05, mean = mu0, sd = sem, lower.tail = FALSE)

# ANOVA ####
#analyze different samples at differenct point in time 
#ex. same people are measured stress level in different point in time 
anova01 <- read.csv(file = "anova01.txt", sep = "\t", header = TRUE)
str(anova01)

# we need two vectors, one with opservations, second with corresponding factors
# (1) generate factor levels
k <- ncol(anova01) # number of treatment levels
n <- nrow(anova01) # number of observations per treatment
f <- factor(names(anova01)) # treatment levels
?gl
tm <- gl(k, 1, n*k, f); tm
# (2) concatenate data rows into single vector
r <- c(t(as.matrix(anova01))) # from example, quite wired use

# apply anova function
av <- aov(r ~ tm)
summary(av) 
# pv = 0.112 > we cannot reject null hypothesis > means are all equal !!
# if we add another factor to annova aov(r ~ tm + [new factor]) it may change
# if we add another data set to annova aov(r ~ tm1 * tm2) it may change

# Non-parametric methods ####
# we are not making any assumption about data distribution (most of parametric methods assume we have normal distribution)
# these methods are more powerfull and robust

# sign.test - used to decide whether a binomial distribution has the equal chance of success and failure.
?binom.test
binom.test(5, 18)

#Wilcoxon test - Two data samples are matched if they come from repeated observations of the same subject. Using the Wilcoxon Signed-Rank Test, we can decide whether the corresponding data population distributions are identical without assuming them to follow the normal distribution.
library(MASS)
str(immer)
?wilcox.test
wilcox.test(immer$Y1, immer$Y2, paired = TRUE) # these sets are not identical

#Mann-Whitney-Wilcoxon Test - Two data samples are independent if they come from distinct populations and the samples do not affect each other. Using the Mann-Whitney-Wilcoxon Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
mtcars$mpg
mtcars$am
wilcox.test(mpg ~ am, data = mtcars) # gas mileage data of manual and automatic transmissions in mtcar are nonidentical populations.

#Kruskal-Wallis Test - A collection of data samples are independent if they come from unrelated populations and the samples do not affect each other. Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution.
kruskal.test(Ozone ~ Month, data = airquality) # monthly ozone density in New York from May to September 1973 are nonidentical populations.
