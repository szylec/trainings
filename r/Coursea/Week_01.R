### Coursera "R programming" week 1

getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea")

rm(list = ls())

# simple types: character, numeric, integer, complex, logical
a = 5
b = 5L

# vector: contain objects of the same class
v = vector() # empty vector
v1 = vector("numeric", length = 10)
vv = c(1,34,4,5435,43,5,4,54,5,45) 
v2 = as.vector(vv, mode = "any")
v3 = as.vector(vv, mode = "list")
v4 = as.list(vv)
?vector

# list: can contain objects of different classes
l = list()

l1 = c(1.7, "a", TRUE)
l = as.list(l1)

x = list(1, "a", TRUE, 1 + 4i)
x

## conversions
x = 0:6
class(x)

as.numeric(x)
as.logical(x)
as.character(x)

# matrix: vectors with a dimension attribute. The dimension attribute is itself an integer vector of length 2 (nrow, ncol)
m = matrix(nrow = 2, ncol = 3);m 

dim(m)
attributes(m)
m[1,1] = "dupa"; m

mm = matrix(data = 1:6, nrow = 2, ncol = 3); mm
mm = matrix(data = 1:6, nrow = 3, ncol = 2); mm
mm = matrix(data = 1:6, nrow = 6, ncol = 1); mm

# binding
x = 1:3
y = 10:12

cbind(x, y)
rbind(x, y)

# factor: used to represent categorical data. Can be ordered and unordered
x = factor(c("yes", "yes", "yes", "no", "yes", "no")); x
table(x)
x = factor(c("yes", "yes", "yes", "no", "yes", "no"), levels = c("yes", "no")); x

# missing values: NA, NaN (only for mathematical issues)
is.na(x)

na = matrix(c(1, 2, 3, NA, NaN, 6), nrow = 2, ncol = 3); na
is.na(na)
is.nan(na)

# Data Frame: tabular data
# - special type of list where every element of the list has to have the same length
# - unlike matrix, DF can store different classes of objects in each column

x = data.frame(foo = 1:4, bar = c(T, F, F, T)); x
nrow(x)
ncol(x)

?data.frame

##########################
## reading data
# read.table, read.csv
# readLines, for reading lines of text file
# source, for R code files (inverse to dump)
# dget, for R code files (inverse to dput)
# load, for reading in saved workspaces
# unserialize, for reading single R objects in binary form

# ALWAYS calculate roughly how much RAM dataset will need
# 1,500,000 rows, 120 columns, all numeric
# 1500000 x 120 x 8 bytes
# 1440000000 bytes / 2^20
# 1373.29 MB / 2^10
# 1.34 GB

# rule of thumb - you need twice as that RAM to load the file into memory

# Trick how to load faster LARGE datasets
initial = read.table("dataset.txt", nrows = 100)
classes = sapply(initial, class)
tabAll = read.table("dataset.txt", colClasses = classes)

##########################
# Connection to the outside world
# file
# gzfile
# url

?url

con = url("http://www.jhsph.edu", "r")
a = readLines(con)
head(a)

##########################
# Subsetting
# [ - return elements of the same class
# [[ - for data frames and lists, for single element
# $ - for data frames and lists by name

c = c("a", "b", "c", "c", "b", "d" )
c1 = c("e", "e", "f", "g", "h", "h" )
l = as.list(c); l
l[2]
x = list(2,"a","b")
typeof(x[[1]])

df = data.frame(first = c, second = c1); df
df[3,2]
df[3,2, drop = FALSE] # - one dimensional objects back

df[[2]]

df$first

#########################
# partial matching

df$f
df["first"]
df[["f", exact = FALSE]]

#########################
# removing NA
c = c("a", "b", NA, "c", "b", NA )
c1 = c("e", NA, "f", NA, "h", "h" )
df = data.frame(first = c, second = c1); df

bad = is.na(c); bad
c[!bad]

good = complete.cases(c); good
c[good]

good2 = complete.cases(df); good2
df[good2,][1:2] # subset of rows are complete

#########################
# vectorized operation
# insted of writing the loops once can use vector operations like in matlab
x = 1:4
y = 5:8
z = x + y; z

z >= 10

w = z / x; w

############
x = 1:4
y = 2
typeof(x + y)

x = c(17,14,4,5,13,12,10)
x[x > 10] = 4
x

#############
## WEEKLY QUIZ
?read.csv()

getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea")
file = "hw1_data.csv"
data = read.csv(file, header = TRUE, sep = ",", dec = ".")
summary(data)

data[47,"Ozone"]

new_data_1 = subset(data, Ozone > 31 & Temp > 90); new_data_1
summary(new_data_1)

new_data_2 = subset(data, Month == 6); new_data_2
summary(new_data_2)

new_data_3 = subset(data, Month == 5); new_data_3
summary(new_data_3)


#############
## WEEKLY Assignment: swirl

#install.packages("swirl")
packageVersion("swirl")

rm(list = ls())
library(swirl)

#install_from_swirl("R Programming")
swirl()

# check the arguments function is using
args(name = list.files)

# create dir in current ws
dir.create("TEST")

