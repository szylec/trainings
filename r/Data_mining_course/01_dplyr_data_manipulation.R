# DATA DISCOVERY #####
# https://bookdown.org/rdpeng/exdata/

## Data discovery task

## aggregate
# txt <- "aaa bbb aaa bbb aaa aaa aaa ccc"
# txt_ul <- unlist(strsplit(txt, " "))
# txt_df <- data.frame(x = txt_ul)
# aggregate(x = txt_df, by = list(unique_value = txt_df$x), FUN = length)

## dplyr
# select: return a subset of the columns of a data frame, using a flexible notation
# filter: extract a subset of rows from a data frame based on logical conditions
# arrange: reorder rows of a data frame
# rename: rename variables in a data frame
# mutate: add new variables/columns or transform existing variables
# summarise / summarize: generate summary statistics of different variables in the data frame, possibly within strata

library(dplyr)
getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/data_extraction")
#setwd("/usr/local/google/home/slechwar/Google Drive/04_Dev/data_extraction")

list.files()
chicago <- readRDS("chicago")

names(chicago)
str(chicago)
head(chicago)
nrow(chicago)
summary(chicago)

# dplyr ####
# select
names(select(chicago, matches("t")))
names(select(chicago, starts_with("p")))

n <- names(chicago)
head(chicago[, n[4:6]])

# filter
head(filter(chicago, dptp > 25))
head(chicago[chicago$dptp > 25, ])
nrow(filter(chicago, is.na(pm10tmean2)))
nrow(chicago[is.na(chicago$pm10tmean2), ])
head(filter(chicago, dptp == 28.625))
head(filter(chicago, dptp > 25 & pm10tmean2 < 47))
head(filter(chicago, dptp > 25 | pm10tmean2 < 47))

# summary, hist boxplot
summary(chicago$city)
summary(chicago$tmpd)
summary(chicago$pm25tmean2)
summary(chicago$pm10tmean2)
hist(chicago$pm10tmean2)

# remove outlier
qt <- quantile(chicago$pm10tmean2, c(0.05,0.95), na.rm = TRUE)
chic_without_outlier <- filter(chicago, chicago$pm10tmean2 >= qt[1] & chicago$pm10tmean2 <= qt[2])
summary(chic_without_outlier)
hist(chic_without_outlier$pm10tmean2)

# filter and show boxplot
x <- as.data.frame(chicago)
x <- filter(x, !is.na(pm10tmean2) & pm10tmean2 != "NA")
str(x)
x <- x[, c("pm10tmean2", "date")]
aggregate(x$pm10tmean2, by = list(months(x$date)), FUN = mean)

boxplot(pm10tmean2 ~ months(date), data = x, main = "XXX", xlab = "x desc", ylab = "y desc")

# arrange
?arrange
head(arrange(chicago, date))
head(arrange(chicago, desc(date)))
head(arrange(chicago, desc(pm10tmean2), pm25tmean2))

# rename
?rename
chicago_renamed <- rename(chicago, pm2_5 = pm25tmean2, pm10 = pm10tmean2, dewpoint = dptp)
head(chicago_renamed, 3)

# mutate
?mutate
chicago_mutated <- mutate(chicago, pm2_5_minus_mean = pm25tmean2 - mean(pm25tmean2, na.rm = TRUE))
chicago_mutated <- filter(chicago_mutated, chicago_mutated$pm2_5_minus_mean != "NA")
head(chicago_mutated)

hist(chicago_mutated$pm2_5_minus_mean)
q <- quantile(chicago_mutated$pm2_5_minus_mean, c(0.05, 0.95))
chicago_mutated <- filter(chicago_mutated, pm2_5_minus_mean >= q[1] & pm2_5_minus_mean <= q[2])
hist(chicago_mutated$pm2_5_minus_mean)

# group_by
?group_by
chicago_mutated <- mutate(chicago, year = as.integer(year(date)))
chicago_mutated <- group_by(chicago_mutated, year)
str(chicago_mutated)

filter(chicago_mutated, year == 1988)
summarise(chicago_mutated, pm2_5 = mean(pm25tmean2, na.rm = TRUE), pm10 = max(pm10tmean2, na.rm = TRUE))

#In a slightly more complicated example, we might want to know what are the 
#average levels of ozone (o3) and nitrogen dioxide (no2) within quintiles of pm25

q <- quantile(chicago$pm25tmean2, c(0.25, 0.5, 0.75), na.rm = TRUE)
q <- quantile(chicago$pm25tmean2, seq(0, 1, 0.2), na.rm = TRUE)
cut(chicago$pm25tmean2, q)
chicago_mutated <- mutate(chicago, pm2_5 = cut(pm25tmean2, q))
chicago_mutated <- group_by(chicago_mutated, pm2_5)
str(chicago_mutated)
for_plot <- summarise(chicago_mutated, o3 = mean(o3tmean2, na.rm = TRUE), no2 = mean(no2tmean2, na.rm = TRUE))

plot(for_plot$pm2_5, for_plot$o3)
plot(for_plot$pm2_5, for_plot$no2)

# %>% (pipeline operator)
# create a new variable pm25.quint
# split the data frame by that new variable
# compute the mean of o3 and no2 in the sub-groups defined by pm25.quint
q <- quantile(chicago$pm25tmean2, seq(0, 1, 0.1), na.rm = TRUE)
chicago_mutated <- mutate(chicago, pm25.q = cut(pm25tmean2, q)) %>%
  group_by(pm25.q) %>%
  summarise(o3 = mean(o3tmean2, na.rm = TRUE), no2 = mean(no2tmean2, na.rm = TRUE))

# Another example might be computing the average pollutant level by month. 
# This could be useful to see if there are any seasonal trends in the data.
months(chicago$date)
month(chicago$date)

for_plot <- mutate(chicago, month = as.integer(month(as.POSIXlt(chicago$date)))) %>%
  group_by(month) %>%
  summarise(pm25 = mean(pm25tmean2, na.rm = TRUE),
            pm10 = mean(pm10tmean2, na.rm = TRUE),
            o3 = mean(o3tmean2, na.rm = TRUE),
            no2 = mean(no2tmean2, na.rm = TRUE))
## ?write.csv

# Exploratory analysis ####
rm(list = ls())
list.files()
getwd()
path <- paste0(getwd(), "/data")
setwd(path)

ozone <- read.csv("hourly_44201_2014.csv")
head(ozone)
nrow(ozone)
ncol(ozone)
names(ozone) <- make.names(names(ozone))
str(ozone)
ozone$POC <- as.factor(ozone$POC)
summary(ozone)

ozone[, "id"] <- 1:nrow(ozone)

# percentage of NA's in each column
colMeans(is.na(ozone)) * 100

# get number of levels of POC
ozone %>%
  group_by(POC) %>%
  summarise(nn = n_distinct(id), na.rm = TRUE)

ozone %>%
  group_by(POC) %>%
  summarise(nn = n(), na.rm = TRUE)

# check your measurement
summary(ozone$Sample.Measurement)
boxplot(ozone$Sample.Measurement)
hist(ozone$Sample.Measurement)

# cleaning
qq <- quantile(ozone$Sample.Measurement, c(0.01, 0.99), na.rm = TRUE)
ozone_clean <- ozone %>% filter(Sample.Measurement >= qq[1] & Sample.Measurement <= qq[2])
hist(ozone_clean$Sample.Measurement)

# q: Which counties in the United States have the highest levels of ambient ozone pollution?
data <- ozone_clean %>% select(id, State.Name, County.Code, County.Name, Date.GMT, Time.GMT, Sample.Measurement)
result <- data %>% group_by(State.Name, County.Name) %>% 
  summarise(meas_avg = mean(Sample.Measurement, na.rm = TRUE),
            meas_med = median(Sample.Measurement, na.rm = TRUE),
            meas_max = max(Sample.Measurement, na.rm = TRUE))
arrange(result, desc(meas_avg))
arrange(result, desc(meas_med))
arrange(result, desc(meas_max))

ranking <- group_by(ozone, State.Name, County.Name) %>%
  summarize(ozone = mean(Sample.Measurement)) %>%
  as.data.frame %>%
  arrange(desc(ozone))

head(ranking, 10)

# take a look at months disctibution of ozone in most polluted county
result <- ozone_clean %>% 
  filter(State.Name == "Colorado" & County.Name == "Clear Creek") %>%
  select(State.Name, County.Name, Date.Local, Sample.Measurement) %>%
  mutate(Date.Local = as.Date(Date.Local)) %>%
  group_by(month = month(Date.Local)) %>%
  summarise(ozone = mean(Sample.Measurement, na.rm = TRUE))

plot(result$month, result$ozone, type = "l")

# q: Are hourly ozone levels on average higher in New York City than they are in Los Angeles?
str(ozone_clean)

result <- ozone_clean %>%
  filter(County.Name == "New York" | County.Name == "Los Angeles") %>% 
  select(State.Name, County.Name, Date.Local, Sample.Measurement) %>%
  mutate(month = month(Date.Local)) %>%
  group_by(State.Name, County.Name, month) %>%
  summarise(ozone = mean(Sample.Measurement, na.rm = TRUE))

ny <- result %>% filter(County.Name == "New York")
la <- result %>% filter(County.Name == "Los Angeles")

plot.new()
plot.window(xlim = range(ny$month), ylim = range(ny$ozone))
lines(ny$month, ny$ozone, col = "red")
axis(1); axis(2); box()
plot.window(xlim = range(la$month), ylim = range(la$ozone))
lines(la$month, la$ozone, col = "blue")
axis(1); axis(2); box()
