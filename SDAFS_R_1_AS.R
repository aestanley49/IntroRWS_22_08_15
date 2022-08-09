#########CHAPTER 1###############################

# Four panels in R
# Console- Directly enter code/ where output ends up
# Plotting window- Displays plots/ install packages/ read help files
# Environment- Where the data is stored and can be accessed
# Source (Script)- Maintain a permanent record of code

###1.1.1###########
# R as a calculator; assigning objects to values
3+3

12/4

# Create an object (x) and assign it a value
x = 3 + 3

x

y <- 2+5

# Perform computations with objects
z <-  y - x

#**A These have all been numbers so far, but there are other forms of data, right like dates

###################

###1.2#############
# Saving R code in scripts
# File --> New File --> R script
###################

#**AS - introduce the idea of folders - R uses the folder system on your computer, you just have to tell it where/what that is

###1.3#############
# The working directory
getwd()
#setwd("C:/Users/YOU/Documents/R-Book/Chapter1")# example of working directory location
# Go to session --> set working directory --> source file location
# Go to session --> set working directory --> choose directory
###################

###1.4#############
# R object types (Functions/vectors/matrices/data frames)
# Functions
fun(arg1 = value1, arg2 = value2)

fun(value1, value2) # removes the need to specify argument names
y2 = sum(2, 5)
# same thing as above - y <- 2+5

print(x = z)

#Vectors
# : represents 2-6
(month <-  c(2,3,4,5,6)) 
(month <-  2:6) 
month

#! This won't work:
(month <-  2,3,4,5,6) 
month

day <-  seq(1,9,2); 
day

year <-  rep(2018, 5); year

length(year)

# Can create vectors with your own input
(number <-  c(4, 7, 8, 10, 15))

pond <-  c("F11", "S28", "S30", "S8", 'S11'); pond

# Data class ( difference between numeric vector and character vector)

class(pond)
class(number)

v = c(1,2,3,"a"); v
# Notice that each element in the vector was coerced to be a character

sum(v)

#Matrices
# Column bind- Combines the vectors as columns
m1 <-  cbind(month, day, year, number); m1

# Row bind- Combines the vectors as rows
m2 <-  rbind(month, day, year, number); m2

cbind(m1, pond)
# Notice that because 'pond' contained a character vector that the whole matrix is coerced to be character

#Data frames
df1 <-  data.frame(month, day, year, number, pond); df1

class(day); class(pond); class(m1); class(df1)
###################

###1.5#############
# Factors in R
class(df1$pond)

df1$pond <- as.factor(df1$pond)
# Factor class- Treats these data as grouping variable

df1$pond

levels(df1$pond)

# Can view the structure of a data frame
str(df1)

###################

###1.6#############
# Vector math in R
dm <-  day + month; dm
# Vectors need to be of the same size in order to perform computations
# The same is true for matrices; Matrices must be of the appropriate size

dm/2
###################

###1.7#############
# Data subsetting and queries (Index/name/logical)
day
day[3]     # By index

m1
m1[1,4]
# For a matrix, [x,y] represents the value in the xth row and yth column

df1
df1[,1]
# [,y] means take all rows of the yth column
# [x,] means take all columns from the xth row

m1
m1[c(1,2,4),]

df1$month       # By name

# Can combine methods
df1$month[3]

df1[,c("year", "month")]

df1$dm <-  df1$day + df1$month; df1

# Logical subsetting (in later section)
###################

### Exercise 1A####
Lake <- c('Big', 'Small', 'Square', 'Circle')
Area <- c(100, 25, 45, 30)
Time <- c(1000, 1200, 1400, 1600)
Fish <- c(643, 203, 209, 15)

df <- data.frame(Lake, Area, Time, Fish)

df[2,]

df[,2]

df[c(1,3),4]
###################

###1.8#############
# Reading external data files

dat <-  read.csv("Path/To/FileName.csv")

dat <-  read.csv("../Data/streams.csv")

dat <-  read.csv("streams.csv")
dat
# You have to set up working directory
###################

###1.9#############
# Exploring the data set
head(dat)

tail(dat, 10)

# Summary info about each variable in the data set
summary(dat)

length(dat$stream_width)

length(dat$flow)
# R counts that NA

dim(dat) 
nrow(dat)
ncol(dat)

colnames(dat)

mean(dat$stream_width)

mean(dat$flow)
# Will not run because of the NA

# Removes the NA's so that you can use those variables for computations 
mean(dat$flow, na.rm = T)

sum(dat$flow, na.rm = T)/(nrow(dat) - 1)

# Perform functions on multiple variables
apply(dat[,c("stream_width", "flow")], 2, FUN = var, na.rm = T)

# Similar to apply but uses a grouping variable
tapply(dat$stream_width, dat$state, mean)

# Similar to tapply but formats the data in a more convenient way
aggregate(dat$stream_width, by = list(state = dat$state), mean)
###################

###1.10############
# Logical/Boolean operators
# == is asking if x is equal to 5 (TRUE/FALSE statement)
x == 5

x != 5

x < 5

x <= 5

y <-  c(1,2,3)    # In
x %in% y

y <-  c(4,5,6)
x %in% y

y %in% x

x > 4 & x < 6   # And

x <= 5 | x > 5  # Or
###################

###1.11############
# Logical subsetting
# Is stream width > 60
dat$stream_width > 60

# I want the values of flow when the width > 60
dat$flow[dat$stream_width > 60]

# I want the dataframe records where state is Alabama
dat[dat$state == "Alabama",]

dat[dat$state != "Alabama",]
###################

###1.12############ 
# If else statements
if (x == 5) print("x is equal to 5")

if (x != 5) print("x is not equal to 5")

if (x > 5) print("x is greater than 5") else print("x is not greater than 5")

if (x > 5) {
  print("x is greater than 5")
} else {
  print("x is not greater than 5")
} 

xs <-  c(-5:-1, 1:5)

if (xs < 0) print("negative") else print("positive")
# If else statements can only be used for vectors of length one

ifelse(xs > 0, "positive", "negative")
# Allows for vectors of greater length: vector, do if true, do if false

cbind(
  xs,
  ifelse(xs > 0, "positive", "negative")
)

dat$size_cat <-  ifelse(dat$stream_width > 80, "big", "small")
head(dat)

dat$size_cat_fine <-  ifelse(dat$stream_width <= 40, "small",
                           ifelse(dat$stream_width > 40 & dat$stream_width <= 80, "medium", "big"))
# Only needs two ifelse statements even though there are three options

dat
###################

###1.13############
# Writing to output files
write.csv(dat, "updated_streams.csv")

# Writes an R object to an external file
save(dat, file = "updated_streams")

rm(dat); 
head(dat)  # should give error
load(file = "updated_streams")
head(dat) # should show first 6 rows
###################

###1.14############
# User defined functions
myfun <-  function(arg1) {
  # function body goes here
  # use arg1 to do something
  
  # return something as last step
}

power <-  function(x, y){
  x^y
}

power(x = 5, y = 3)

power(power(5,2),2)
###################

###Exercise 1B#####
data <- read.csv('ponds.csv')

summary(data)

tapply(data$chl.a, data$pond, mean)

tapply(data$chao, list(data$pond, data$treatment), mean, na.rm= T)

apply(data[data$pond == "S.28", c("daph", "bosm", "cope", "chao")], 2, var, na.rm = T)

data$prod <-  ifelse(data$chl.a > 30, "high", "low")

table(data$prod, data$treatment)

product <-  function(a,b) {
  a * b
}
product(4,5)

product <-  function(a,b,z) {
  result <-  a * b
  
  if (result <= z) {
    cat("The result of a * b is less than", z, "so you don't care what it is")
  } else {
    cat("The result of a * b is", result, "\n")
    result
  }
}
#################################################