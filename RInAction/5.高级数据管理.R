# 1. Functions
## 1.1 mathmatic funcs
trunc(5.99) # get the integer part, the result is 5
round(3.475, digits = 2) # get 2 float number, the result is 3.48
signif(3.475, digits = 2) # get 2 position of a number, the result is 3.5
## 1.2 Statistics funcs
### removing the frist and the last 0.01 and get the average 
z <- mean(sort(Science, decreasing = F), trim = 0.01, na.rm = TRUE) 
### get the n% of a vector
quantile(Science, c(.3,.84))
### use scale() to normalize data: default is mean = 0, std = 1
scale(Science)
#### change the std to 10, and the mean to 50
newdata <- transform(Science, myvar = scale(myvar)*10 + 50)
### range()
range(c(1,2,3,4)) # the result is c(1,4)
### diff()
diff(range(c(1,2,3,4))) # the result is 3
diff(c(1,5,23,29), lag=2) # the result is 22 24
## 1.3 probability funcs
set.seed(123) # which is used to make results reproducible
runif(5) # generate 5 random numbers which are distributed as normal deveiate
## 1.4 character funcs
x <- c("ab", "cde", "fghij")
### 1.4.1 the length of a string
nchar(x[3]) # the result is 5
### 1.4.2 extract or replace a string
x <- "abcdef"
substr(x, 2, 4) # the result is bcd
substr(x, 2, 4) <- "22222" # x now is a222ef
### 1.4.3 search in a string
#### 1.4.3.1 exact-search
grep("A", c("b", "A", "cdf"), fixed = TRUE) # the result is 2
#### 1.4.3.2 regx-search
grep("/d*/", c("b", "A", "cdf"), fixed = FALSE) # the result is 3
### 1.4.4 search and replace a string
#### fixed=FALSE means exact-search, otherwise we use regx
sub("\\s", ".", "Hello World") # the result is Hello.World
### 1.4.5 split
#### fixed=FALSE means exact-search, otherwise we use regx
strsplit("abc", "") # the result is "a" "b" "c"
### 1.4.6 concatenate strings
paste("x", 1:3, sep = "M") # the result is c("xM1", "xM2", "xM3")
## 1.5 apply funcs to matrix
mydata <- matrix(rnorm(30), nrow = 6)
apply(mydata, 1, mean) # 1 means each row, 2 means each column
apply(mydata, 2, mean, trim=0.2) # reserve the middle 60%

# 2. Exercise
options(digits = 2)
Student <- c("john Davis", "Angela Williams", "Bullwinkle Moose", "David Jones", 
              "Janice Markhammer", "Cheryl Cushing", "Reuven Ytzrhak", "Greg Knox",
              "Joel England", "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English, stringsAsFactors = FALSE)

## 计算综合得分
z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)

## set grades for students
y <- quantile(score, c(.8,.6,.4,.2))
roster <- within(roster, {
                grade <- NA
                grade[score >= y[1]] <- "A"
                grade[score < y[1] & score >= y[2]] <- "B"
                grade[score < y[2] & score >= y[3]] <- "C"
                grade[score < y[3] & score >= y[4]] <- "D"
                grade[score < y[4]] <- "F"
                })

## seperate names to fname and lname
name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname, Lastname, roster[, -1])

## ordering
roster <- roster[order(Lastname, Firstname),]

# 3. Aggregate & Reshape
cars <- mtcars[1:5, 1:4]

## 3.1 Transpose 
t(cars)
## 3.2 Collapse
options(digits = 3)
attach(mtcars)
aggdata <- aggregate(mtcars, by=list(cyl, gear), FUN=mean, na.rm=TRUE)
detach(mtcars)
## 3.3 reshape2
ID <- c(1,1,2,2)
Time <- c(1,2,1,2)
X1 <- c(5,3,6,2)
X2 <- c(6,5,1,4)
mydata <- data.frame(ID, Time, X1, X2, stringsAsFactors = FALSE)

### 3.3.1 melt
### (format: ID-varible): for each row, it only has Primary key(s), 1 varibale name and its value
library(reshape2)
md <- melt(mydata, id=c("ID", "Time"))
### 3.3.2 cast
#### column name~value
#### show ID, and variable's means
dcast(md, ID~variable, mean)
#### show Time and variable's means
dcast(md, Time~variable, mean)
#### show ID and time's mean
dcast(md, ID~Time, mean)
#### show ID, Time and according variables' values
dcast(md, ID+Time~variable)
#### show ID, variables, and according Times' values
dcast(md, ID+variable~Time)
#### show ID, variables and show value matrix
dcast(md, ID+variable~value)
#### show ID, and values according to variable and time 
dcast(md, ID~variable+Time)
