### Coursera "R programming" week 1

getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea")

rm(list = ls())

# if, else: testing a condition
# for: execute a loop a fixed number of times
# while: execute a loop while a condition is true
# repeat: execute an infinite loop
# break: break the execution of a loop
# next: skip an interation of a loop
# return: exit a function

x <- c("a", "b", "c", "d")

for(i in 1:10) {
  print(x[i])
}

m <- matrix(1:6, 2, 3)
m

for(i in seq_len(nrow(m))) {
  for(j in seq_len(ncol(m))) {
    print(m[i, j])
  }
}

count <- 0

while(count < 10) {
  print(count)
  count <- count + 1
}


# functions

add2 <- function(x,y) {
  x + y ### last expression is return
}

add2(2,5)

above_number <- function(x, y = 10) {
  use <- x > y # logical vector of T and F
  x[use]
}

v1 <- c(1,2,3,456,3,35,63,565,356,65,65,6,6,63,6453,5346,564) 

above_number(v1)
above_number(v1, 5)

column_mean <- function(y, removeNA = TRUE) {
  no_of_col <- ncol(y)
  means <- numeric(no_of_col)
  
  for(i in 1:no_of_col) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  
  means
}

data <- runif(100, 0, 15)
m1 <- matrix(data, nrow = 20, ncol = 5)

column_mean(m1)

# !!! function can be defined in other function
# !!! function can return function

# Date and time
dt <- Sys.time()
quarters(dt)

p <- as.POSIXlt(dt)
unclass(dt)

datastring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datastring, "%B %d, %Y %H:%M")
x

############################
#### practical assignment
############################

rm(list = ls())

#getwd()
#setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea/specdata")
#setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea")
#list.files()


## PART 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  data <- data.frame(Date = as.Date(character()),
                     sulfate = double(),
                     nitrate = double(),
                     ID = integer())
  main_directory <- "/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea/"
  
 for(i in 1:length(id)) {
    id_temp <- id[i]
    id_str <- as.character(id[i])
  
    if(nchar(id_str) == 1) {
      file_name <- capture.output(cat("00", id_str, ".csv", sep = ""))
    } else if(nchar(id_str) == 2) {
      file_name <- capture.output(cat("0", id_str, ".csv", sep = ""))
    } else if(nchar(id_str) == 3) {
      file_name <- capture.output(cat(id_str, ".csv", sep = ""))
    } else {
      file_name <- ""
    }
    
    csv_file <- capture.output(cat(main_directory, directory,"/", file_name, sep = ""))
    data_temp <- read.csv(csv_file, header = TRUE, sep = ",", dec = ".", na.strings = "NA")
    data <- rbind(data, data_temp)
  }

    # calc
  mean(data[,pollutant], na.rm = TRUE)
}

# test, part 1
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate")
pollutantmean("specdata", "nitrate", 23)

## PART 2
complete <- function(directory, id = 1:332) {
  main_directory <- "/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea/"
  complte_cases_data <- data.frame(id = integer(),
                                   nobs = integer())
  
  complte_cases_data_temp <- data.frame(id = integer(),
                                        nobs = integer())
    for(i in 1:length(id)) {
    id_temp <- id[i]
    id_str <- as.character(id[i])
    
    if(nchar(id_str) == 1) {
      file_name <- capture.output(cat("00", id_str, ".csv", sep = ""))
    } else if(nchar(id_str) == 2) {
      file_name <- capture.output(cat("0", id_str, ".csv", sep = ""))
    } else if(nchar(id_str) == 3) {
      file_name <- capture.output(cat(id_str, ".csv", sep = ""))
    } else {
      file_name <- ""
    }
    
    csv_file <- capture.output(cat(main_directory, directory,"/", file_name, sep = ""))
    data_temp <- read.csv(csv_file, header = TRUE, sep = ",", dec = ".", na.strings = "NA")

    complte_cases_data_temp <- data.frame(id = integer(),
                                          nobs = integer())
    
    complte_cases_data_temp[1,"id"] <- id[i]
    complte_cases_data_temp[1,"nobs"] <- sum(as.numeric(complete.cases(data_temp)))
    complte_cases_data <- rbind(complte_cases_data, complte_cases_data_temp)
  }
  complte_cases_data
}

# test, part 2
complete("specdata", 1)
complete("specdata", c(6,10,20,34,100,200,310))
complete("specdata", 30:25)
complete("specdata", 54)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

## PART 3

corr <- function(directory, threshold = 0) {
  main_directory <- "/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea/"
  number_of_files <- length(list.files("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea/specdata"))
  id <- 1:number_of_files
  
  corr_vector <- vector("numeric")
  corr_value <- vector("numeric")
  
  complte_cases_data <- data.frame(id = integer(),
                                   nobs = integer())
  
  complte_cases_data_temp <- data.frame(id = integer(),
                                        nobs = integer())
  
  for(i in 1:length(id)) {
    id_temp <- id[i]
    id_str <- as.character(id[i])
    
    if(nchar(id_str) == 1) {
      file_name <- capture.output(cat("00", id_str, ".csv", sep = ""))
    } else if(nchar(id_str) == 2) {
      file_name <- capture.output(cat("0", id_str, ".csv", sep = ""))
    } else if(nchar(id_str) == 3) {
      file_name <- capture.output(cat(id_str, ".csv", sep = ""))
    } else {
      file_name <- ""
    }
    
    csv_file <- capture.output(cat(main_directory, directory,"/", file_name, sep = ""))
    data_temp <- read.csv(csv_file, header = TRUE, sep = ",", dec = ".", na.strings = "NA")
    
    complte_cases_data_temp <- data.frame(id = integer(),
                                          nobs = integer())
    
    if(sum(as.numeric(complete.cases(data_temp))) > threshold) {
      cor_matrix <- cor(data_temp[,c("sulfate", "nitrate")], use = "complete.obs")
      cor_value <- as.numeric(cor_matrix[1,2])
      corr_vector <- append(corr_vector, cor_value)
    }
  }
  corr_vector
}

cr <- corr("specdata", 2000)  
n <- length(cr)
cr <- corr("specdata", 1000)  
cr <- sort(cr)
#set.seed(197)
#out <- c(n, round(cr[sample(n, 5)], 4))
print(c(n, round(cr, 4)))

summary(cr)

x <- 1:10
if(x > 5) {
  x <- 0
}

