myvars <- c("mpg", "hp", "wt")
mystats <- 
  function(x, na.omit = F) {
    if (na.omit)
      x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x-m)^3/s^3)/n
    kurt <- sum((x-m)^4/s^4)/n -3
    return(c(n = n, mean = m, stdev = s, skew = skew, kutosis = kurt))
  }
sapply(mtcars[myvars], mystats)

# 1. ways to see details
library(Hmisc)
describe(mtcars[myvars])
#install.packages("pastecs")
library(pastecs)
stat.desc(mtcars[myvars], basic = T, desc = T, norm = F, p = 0.95)
#install.packages("psych")
library(psych)
describe(mtcars[myvars])
Hmisc::describe(mtcars[myvars])
## 1.1 data by groups
### aggregate() can only return sigle value like median, std, etc..
aggregate(mtcars[myvars], by = list(am=mtcars$am), mean)
aggregate(mtcars[myvars], by = list(am=mtcars$am), sd)
### by(data, indices, fun): can return multiple values
dstats <- function(x) sapply(x, mystats)
by(mtcars[myvars], mtcars$am, dstats)
## 1.2 doBy
install.packages("doBy")
library(doBy)
### summaryBy(var1+var2 ~ groupvar1 + groupvar2, data=dataframe, FUN = function)
summaryBy(mpg+hp+wt ~ am, data=mtcars, FUN = mystats)

# 2. Table
library(vcd)
# 2.1 1-dim table
mytable <- with(Arthritis, table(Improved))
prop.table(mytable)
prop.table(mytable)*100
# 2.2 2-dim table
## xtabs(~A+B, data = mydata)
## variables needed to be displayed should be at right side
mytable <- xtabs(~Treatment + Improved, data = Arthritis)
margin.table(mytable, 1) 
margin.table(mytable, 2) # 2 means the second variable
prop.table(mytable, 1) 
prop.table(mytable, 2) 
addmargins(mytable)
addmargins(prop.table(mytable)) # sum for all variables
addmargins(prop.table(mytable), 1) # sum for columns, 2 means for rows 
### by default, table() ignores all NAs, if NA should be taken into account
### use table(useNA="ifany")
## 2.2.1 CrossTable()
#install.packages("gmodels")
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)
## 2.3 n-dim table
mytable <- xtabs(~Treatment + Sex + Improved, data = Arthritis)
ftable(mytable)
margin.table(mytable, 1) # the variable Treatment
margin.table(mytable, 2) # the variable Sex
margin.table(mytable, 3) # the variable Improved
margin.table(mytable, c(1, 3))
ftable(prop.table(mytable, c(1,2)))
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100

# 3. Independency Test
## 3.1 Chi-square Test: large dataset, expected frequences >= 5
library(vcd)
mytable <- xtabs(~Treatment + Improved, data = Arthritis)
chisq.test(mytable)
## 3.2 Fisher's Exact Test: small dataset
fisher.test(mytable)
## 3.3 Cochran-Mantel-Haenszel Test
### the independence test between 2 vars based on the other var
mytable <- xtabs(~Treatment + Improved + Sex, data = Arthritis)
mantelhaen.test(mytable)
## 3.4 Mesuring Relevance
library(vcd)
mytable <- xtabs(~Treatment + Improved, data = Arthritis)
assocstats(mytable)

# 4. Correlation Coefficent
## 4.1 cor(x, use = , method = )
### + means positive correlation, - means negative
### value means the strength
### Person: 衡量了两个定量变量之间的线性相关程度
### 等级相关系数，衡量分级定序变量之间的相关程度
### 非参数的等级相关度量 
states <- state.x77[,1:6]
cor(states)
cov(states)
cor(states, method = "spearman")
### Kappa
library(vcd)
kappa(states)
### partial correlation
#### pcor(c, cov(data))
##### the first two values in c are the ones which to be tested
##### the rest are values which should be ignored
#install.packages("ggm")
library(ggm)
pcor(c(1,5,2,3,6), cov(states)) # 偏相关

## 4.2 t-test

