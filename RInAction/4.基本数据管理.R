manager <- c(1,2,3,4,5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors = FALSE)

# 1. adding new var to data frame
mydata <- data.frame(x1 = c(2, 2, 6, 4),
                     x2 = c(3, 4, 2, 8))
mydata <- transform(mydata, 
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2)

# 2. Re-coding 
leadership$age[leadership$age == 99] <- NA
#leadership$agecat[leadership$age > 75] <- "Elder"
#leadership$agecat[leadership$age >= 55 & leadership$age <=75] <- "Middle Aged"
#leadership$agecat[leadership$age < 55] <- "Young"

# 以上代码等价于下列代码
# within: it allows you to modify the dataframe, i.e., adding the agecat to the dataframe
leadership <- within(leadership, {
                     agecat <- NA
                     agecat[age > 75] <- "Elder"
                     agecat[age >= 55 & age <= 75] <- "Middle Aged"
                     agecat[age < 55] <- "Young"
})

# 3. 变量重命名
names(leadership)[6:10] <- c("item1", "item2", "item3", "item4", "item5")
library(plyr)
leadership <- rename(leadership, c(manager="managerID", date="testDate"))
names(leadership)[2] <- "date"

# calculating without NAs
x <- c(1, 2, NA, 3)
y <- sum(x, na.rm = TRUE)
# remove all NA values
newLeadership <- na.omit(leadership)

# 4. format the date values
dateFormate <- "%m/%d/%y"
leadership$testDate <- as.Date(leadership$testDate, dateFormate)
# calculation of dates
today <- Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%A")
startdate <- as.Date("2004-02-13")
enddate <- as.Date("2011-01-22")
days <- enddate - startdate
difftime(today, as.Date("1956-10-12"), units = "weeks")
diff_day <- difftime(today, as.Date("1956-10-12"), units = "days")

# 5. dataframe ordering
attach(leadership)
newdata <- leadership[order(gender, -age), ]
detach(leadership)

# 6. dataframe merging
## 6.1 merging by one or more common variables
total <- merge(dataframeA, dataframeB, by = c("ID", "Country"))
## 6.2 directly combining two dataframes
### 6.2.1 cbind(): data in each dataframe is displayed as columns
cbind(A, B)
## 6.2.2 rbind(): dataframe1 is displayed above dataframe2
rbind(A, B)

# 7. Subset of dataframes
## 7.1 get subset
newdata <- leadership[, c(6:10)]
myvars <- c("q1", "q2", "q3")
### the easier but equivalent way is as follows:
myvars <- paste("q", 1:3, sep = "")
newdata <- leadership[myvars]
## 7.2 delete variables
myvars <- names(leadership) %in% c("q3", "q4")
newdata <- leadership[!myvars]
### if know the index
newdata <- leadership[c(-8, -9)]
leadership$q3 <- leadership$q4 <- NULL

# 8. get targeted dataset
attach(leadership)
newdata <- leadership[gender=='M' & age > 30]
detach(leadership)
## compare dates
leadership$testDate <- as.Date(leadership$date, "%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$date >= startdate & leadership$date <= enddate),]
## subset()
newdata <- subset(leadership, age >= 35 | age < 24, select = c(q1, q2, q3, q4)) # reserve q1, q2, q3, q4
newdata <- subset(leadership, gender == "M" & age > 25, select = gender:q4) # reserve columns from gender to q4
## sampling
mysample <- leadership[sample(1:nrow(leadership), 3, replace = FALSE),]
### 3 means the size of the sample

# 9. Using SQL to manipulate dataframe: library sqldf
library(sqldf)
newdf <- sqldf("select * from mtcars where carb = 1 order by mpg", row.names = TRUE)
newdf
sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from mtcars
      where cyl in (4, 6) group by gear")
