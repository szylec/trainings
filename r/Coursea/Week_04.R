### Coursera "R programming" week 4

###################################
## Assignment
###################################
library(dplyr)
rm(list = ls())
getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/R_cources/Coursea/rprog data ProgAssignment3-data")


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}

## Assignment 3 > function "rankall"
rankall <- function(outcome_name, num) {
  # outcome > co chcemy zobaczy?? spo??r??d 3 mo??liwych
  # num > czy chcemy zobaczy?? najlepsze, najgorszsze czy o danym rankingu
  # "remisty" tak samo jak w poprzednim (alfabetycznie)
  # pokazujemy warto??ci dla wszystkich stan??w, je??li nie ma szpitala to NA
  # walidacja tak samo jak w poprzednim
  #-----------------------------------------
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome and num are valid
  list_of_outcomes <- c("heart attack", "heart failure", "heart pneumonia")
  list_of_rankings <- c("best", "worst")
  
  is_valid_outcome <- !is.na(match(outcome_name, list_of_outcomes))
  is_valid_num_best_worst <- !is.na(match(num, list_of_rankings))
  is_valid_num_number <- is.numeric(num)
  
  if(!is_valid_outcome) {
    paste0("Error in rankall: invalid outcome")
  } else if(!is_valid_num_number && !is_valid_num_best_worst) {
    paste0("Error in rankall: invalid result")
  } else {
    ## For each state, find the hospital of the given rank
    if(outcome_name == "heart attack") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome_name == "heart failure") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome_name == "heart pneumonia") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    data <- data.frame(hospital_name = outcome[, "Hospital.Name"],
                       state = outcome[, "State"],
                       measure = as.numeric(gsub("Not Available", NA, outcome[, measure_name])))
    
    data <- split(data, data$state)
    output <- data.frame(state = unique(outcome[, "State"])) # at first, only list of states
    
    #todo: iterate throuh all data (splitted states)
    result <- data.frame(hospital_name = as.character(),
                      state = as.character())
    
    for(i in 1:length(data)) {
      tmp <- as.data.frame(data[i])
      colnames(tmp) <- c("hospital_name", "state", "measure")
      if(is_valid_num_best_worst) {
        if(num == "best") {
          tmp <- arrange(filter(tmp, !is.na(measure)), measure, hospital_name)
        } else {
          tmp <- arrange(filter(tmp, !is.na(measure)), desc(measure), hospital_name)
        }
        res <- tmp[1, c("hospital_name", "state")]
      } else if(is_valid_num_number) { #OK
        tmp <- arrange(tmp, measure, hospital_name)
        tmp[, "order"] <- order(tmp$measure)
        tmp[is.na(tmp$measure), "order"] <- NA
          #arrange(tmp, desc(measure), hospital_name)[1, "order"] + 1
        res <- select(filter(tmp, order == num), hospital_name, state)
      }
      result <- rbind(result, res)
    }
    
    output <- merge(result, output, by.x = "state", by.y = "state", all.y = T)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(output)  
  }
}

head(rankall("heart attack", 2), 10)
tail(rankall("heart pneumonia", "worst"), 3)
tail(rankall("heart failure", 10), 10)


r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("heart pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)


## Assignment 2 > function "rank hospital"
rankhospital <- function(state, outcome_name, ranking) {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state, outcome and ranking are valid
  list_of_states <- unique(outcome[, "State"])
  list_of_outcomes <- c("heart attack", "heart failure", "heart pneumonia")
  list_of_rankings <- c("best", "worst")
  
  is_valid_state <- match(state, list_of_states)
  is_valid_outcome <- match(outcome_name, list_of_outcomes)
  is_valid_ranking <- match(ranking, list_of_rankings)
  
  if(is.na(is_valid_state)) {
    cat(paste0("Error in best(\"", state, "\", \"", outcome_name, "\", \"", 
               ranking,  "\"): invalid state"))
  } else if(is.na(is_valid_outcome)) {
    cat(paste0("Error in best(\"", state, "\", \"", outcome_name, "\", \"", 
               ranking,  "\"): invalid outcome"))
  } else if(!is.numeric(ranking) && is.na(is_valid_ranking)) {
    cat(paste0("Error in best(\"", state, "\", \"", outcome_name, "\", \"", 
               ranking,  "\"): invalid result"))
  } else {
    ## Return hospital name in that state with lowest 30-day death rate  
    if(outcome_name == "heart attack") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome_name == "heart failure") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome_name == "heart pneumonia") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }

    data <- data.frame(hospital_name = outcome[, "Hospital.Name"],
                       state = outcome[, "State"],
                       measure = as.numeric(gsub("Not Available", NA, outcome[, measure_name])))
    
    data <- split(data, data$state)
    data <- get(state, data)
    data <- data[!is.na(data$measure), ]
    
    if(ranking == "best") {
      result <- data[which.min(data$measure), ] 
      result <- as.vector(sort(result$hospital_name)[1])
    } else if(ranking == "worst") {
      result <- data[which.max(data$measure), ]
      result <- as.vector(sort(result$hospital_name)[1])
    } else {
      result <- data[with(data, order(data$measure, data$hospital_name)), ]
      result["unique_pos"] <- seq.int(1:nrow(result))
      result <- as.vector(result[result$unique_pos == ranking, "hospital_name"])
    }
    if(length(result) == 0)
      return(NA)
    else
      return(result)
  }
}

rankhospital("TX", "heart attack", "worst")
rankhospital("TX", "heart attack", "best")
rankhospital("TX", "heart attack", NA)
rankhospital("TX", "heart attack", 20)
rankhospital("TX", "heart failure", 4) ### should be only one: "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000) ### should be NA


rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "heart pneumonia", 10)
rankhospital("NY", "heart attack", 7)

## Assignment 1 > function "best"
best <- function(state, outcome_name) {
  
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  list_of_states <- unique(outcome[, "State"])
  list_of_outcomes <- c("heart attack", "heart failure", "heart pneumonia")
  
  is_valid_state <- match(state, list_of_states)
  is_valid_outcome <- match(outcome_name, list_of_outcomes)
  
  if(is.na(is_valid_state)) {
    cat(paste0("Error in best(\"", state, "\", \"", 
               outcome_name, "\") : invalid state"))
  } else if(is.na(is_valid_outcome)) {
    cat(paste0("Error in best(\"", state, "\", \"", 
               outcome_name, "\") : invalid outcome"))
  } else {
    ## Return hospital name in that state with lowest 30-day death
    ## rate  
    if(outcome_name == "heart attack") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome_name == "heart failure") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome_name == "heart pneumonia") {
      measure_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    data <- data.frame(hospital_name = outcome[, "Hospital.Name"],
                       state = outcome[, "State"],
                       measure = as.numeric(gsub("Not Available", NA, outcome[, measure_name])))
    data <- split(data, data$state)
    data <- get(state, data)
    data <- data[!is.na(data$measure), ]
    result <- data[which.min(data$measure), ]
    result <- as.vector(sort(result$hospital_name)[1])
    
    if(length(result) == 0)
      return(NA)
    else
      return(result)
  }
}


best("NY", "heart attack")
best("MD", "heart attack")


best("SC", "heart attack")
best("NY", "heart pneumonia")
best("AK", "heart pneumonia")



###################################
## ALL THE REST
###################################
getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea")

rm(list = ls())

x <- c(1,2,3,4,4,4,4,5,5)
str(x)
summary(x)

str(lm)

# random numbers

# rnorm: generate random Normal variates with a given mean and standard deviation
# dnorm: evaluate the Normal probability density (with a given mean/SD) at a point (or vector of points)
# pnorm: evaluate the cumulative distribution function for a Normal distribution
# rpois: generate random Poisson variates with a given rate

# dnorm(x, mean = 0, sd = 1, log = FALSE)
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# rnorm(n, mean = 0, sd = 1)

# random linear model

set.seed(1)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2*x + e

summary(y)
plot(x, y)

######################
# profiling

# user time: time charged to the CPU(s) for this expression
# elapsed time: "wall clock" time

# Usually, the user time and elapsed time are relatively close, for straight computing tasks
# Elapsed time may be greater than user time if the CPU spends a lot of time waiting around
# Elapsted time may be smaller than the user time if your machine has multiple cores/processors
# (and is capable of using them)
# Multi-threaded BLAS libraries (vecLib/Accelerate, ATLAS, ACML, MKL)
# Parallel processing via the parallel package

system.time(readLines("http://www.jhsph.edu"))

# Using system.time() allows you to test certain functions or code blocks to see if they are taking
# excessive amounts of time
# Assumes you already know where the problem is and can call system.time() on it
# What if you don???t know where to start?

#### R profiler
# Rprof() keeps track of the function call stack at regularly sampled intervals and tabulates how
# much time is spend in each function
# Default sampling interval is 0.02 seconds
# NOTE: If your code runs very quickly, the profiler is not useful, but then you probably don't need it
# in that case

# The summaryRprof() function tabulates the R profiler output and calculates how much time is
# spend in which function
# There are two methods for normalizing the data
# "by.total" divides the time spend in each function by the total run time
# "by.self" does the same but first subtracts out time spent in functions above in the call stack

#### Summary

# Rprof() runs the profiler for performance of analysis of R code
# summaryRprof() summarizes the output of Rprof() and gives percent of time spent in each
# function (with two types of normalization)
# Good to break your code into functions so that the profiler can give useful information about
# where time is being spent
# C or Fortran code is not profiled


## Quiz
set.seed(1)
rpois(5, 2)


set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e

plot(x, y)


library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
summaryRprof()


###
library(swirl)
swirl()














