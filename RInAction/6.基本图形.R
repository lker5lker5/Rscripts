#install.packages("vcd")
library(vcd)

# 1. bar chart
counts <- table(Arthritis$Improved)
barplot(counts,
        main="Simple Bar Plot",
        xlab="Improvement", ylab="Frequency")
plot(Arthritis$Improved, horiz=T, main = "Horizontal Bar Plot",
        xlab = "Frequency", ylab = "Improved")

## 1.2 stacked bar plot
counts <- table(Arthritis$Improved, Arthritis$Treatment)
barplot(counts, main = "Stacked Bar Plot",
        xlab = "Treatment", ylab = "Frequency",
        col = c("red", "yellow", "green"),
        legend = row.names(counts))
barplot(counts, main = "Stacked Bar Plot",
        xlab = "Treatment", ylab = "Frequency",
        col = c("red", "yellow", "green"),
        legend = row.names(counts), beside = T)
linedata <- as.data.frame(counts)

## 1.3 mean barchat
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN = mean)
means <- means[order(means$x), ]
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

## 1.4 adjustment of barchart
opar <- par(no.readonly = T)
par(las = 2)
counts <- table(Arthritis$Improved)
barplot(counts, 
        main = "Treatment Outcome",
        horiz = T, 
        cex.names = 0.8,
        names.arg = c("No Improvement", "Some Improvement", "Marked Improvement"))

## 1.5 Spinogram
library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = "Sponogram Example")
detach(Arthritis)

# 2. Histogram
opar <- par(no.readonly = T)
par(mfrow = c(2, 2))
hist(mtcars$mpg)

hist(mtcars$mpg, breaks = 12,
     col = "red", xlab = "Miles Per Gallon",
     main = "Colored Histogram with 12 bins")

hist(mtcars$mpg, freq = F, col = "red",
     xlab = "Miles Per Gallon", 
     main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = "blue", lwd = 2)

x <- mtcars$mpg
h <- hist(x, breaks = 12, col = "red", 
          xlab = "Miles Per Gallon", 
          main = "Histogram with Normal Curve and Box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
box()
par <- opar

# 4. kernel density
opar <- par(no.readonly = T)
par(mfrow = c(2, 1))
d <- density(mtcars$mpg)
plot(d, main = "Kernel Density of Miles Per Gallon")
polygon(d, col = "red", border = "blue")
rug(mtcars$mpg, col = "brown")
par <- opar

## 4.1 Comparable Kernel Density
#install.packages("sm")
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels = c(4, 6, 8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder"))
sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")
title(main = "MPG Distribution by Car Cylinders")
colfill <- c(2:(1+length(levels(cyl.f))))
legend("topright", levels(cyl.f), fill = colfill)
detach(mtcars)
par <- opar

# 5. boxplot
boxplot(mtcars$mpg, main = "Box Plot", ylab = "Miles per Gallon")
boxplot.stats(mtcars$mpg)
## 5.1 Comparable boxplot
boxplot(mpg ~ cyl, data = mtcars,
        main = "Car Mileage Data",
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon")
## 5.2 boxplot with notch
boxplot(mpg ~ cyl, data = mtcars,
        notch = T, varwidth = T,
        col = "red",
        main = "Car Mileage Data", 
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon")
## 5.3 boxplot with different variables
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels = c(4, 6, 8),
                       labels = c("4", "6", "8"))
mtcars$am.f <- factor(mtcars$am,
                      levels = c(0,1),
                      labels = c("auto", "standard"))
boxplot(mpg ~ am.f * cyl.f,
        data = mtcars,
        varwidth = T,
        col = c("gold", "darkgreen"),
        main = "MPG Distribution by Auto Type",
        xlab = "Auto Type", ylab = "Miles Per Gallon")
## 5.4 violin plot
#install.packages("vioplot")
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x1, x2, x3,
        names = c("4 cyl", "6 cyl", "8 cyl"),
        col = "gold")
title("Violin Plots of Miles Per Gallon", ylab = "Miles Per Gallon",
      xlab = "Number of Cylinders")

# 6. Dot chart
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = .7,
         main = "Gas Mileage for Car Models",
         xlab = "Miles Per Gallon")
## 6.1 Enhanced dotchart
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x), # get names from the dataframe
         cex = .7,
         groups = x$cyl, # groups based on cyl
         gcolor = "black", # color for the number 4, 6, 8
         color = x$color,
         pch = 19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon"
         )
