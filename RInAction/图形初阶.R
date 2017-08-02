# 1. The use of graphs
## 1.1 save the graph
pdf("mygraph.pdf") #png(), jpeg(), bmp(), tiff() and so on
attach(mtcars)
plot(wt,mpg)
abline(lm(mpg~wt)) # a line fits dots
title("Regression of MPG on Weight")
detach(mtcars)
dev.off()
## 1.2 prevent overriding graphs
dev.new()
plot(mtcars)
dev.new()
plot(wt,mpg)
### another way on Mac is pressing "[" "]" to switch graphs
### using dev.new(), dev.next(), dev.prev(), dev.set(), dev.off() to open graphs

# 2. An Example
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type = "b")

# 3. Graph paras
## 3.1 backup old paras
opar <- par(no.readonly = T)
par(lty=2, pch=17)
plot(dose, drugA, type = "b")
par(opar)
### <=>
plot(dose, drugA, type = "b", lty = 2, pch = 17)
## 3.2 icons and lines
plot(dose, drugA, type = "b", lty = 3, lwd = 3, pch = 15, cex = 2)
## 3.3 colors
### col=1,col="white", col="#FFFFFF", col=rgb(1,1,1), col=hsv(0,0,1)
### rainbow(), heat.colors(), terrain.colors(), topo.colors(), cm.colors()
library(RColorBrewer)
mycolors <- brewer.pal(7, "Set1")
barplot(rep(1,7), col = mycolors)
## 3.4 paras to control graphs
opar <- par(no.readonly = T)
par(pin=c(2,3))
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
plot(dose, drugA, type = "b", pch = 19, lty = 2, col = "red")
plot(dose, drugB, type = "b", pch = 23, lty = 6, col = "blue", bg = "green")
par(opar)
## 3.5 legend
plot(dose, drugA, type = "b",
     col = "red", lty = 2, pch = 2, lwd = 2,
     main = "Clinical Trials for Drug A", 
     sub = "This is hypothetical data",
     xlab = "Dosage", ylab = "Drug Response",
     xlim = c(0, 60), ylim = c(0, 70))
## 3.6 custom axis
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = T)
par(mar=c(5,4,4,8) + 0.1)
plot(x, y, type = "b",
     pch = 21, col = "red",
     yaxt = "n", lty = 3, ann = F)
lines(x, z, type="b", pch = 22, col = "blue", lty = 2 )
axis(2, at = x, labels = x, col.axis = "red", las = 2)
axis(4, at = z, labels = round(z, digits = 2), 
     col.axis = "blue", las = 2, cex.axis = 0.7, tck = -.01)
mtext("y=1/x", side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")
title("An Example of Creative Axes", xlab = "X values", ylab = "Y=X")
library(Hmisc)
minor.tick(nx=2, ny=2, tick.ratio = .5)
par(opar)

## 3.7 reference line
abline(h=c(1,5,7)) # add reference line at y = 1, 5, 7
abline(v=seq(1, 10, 2), lty = 2, col = "blue")
### 3.7.1 example
opar <- par(no.readonly = T)
par(lwd = 2, cex = 1.5, font.lab = 2)
plot(dose, drugA, type = "b", 
     pch = 15, lty = 1, col = "red", ylim = c(0,60),
     main = "Drug A vs. Drug B", 
     xlab = "Drug Dosage", ylab = "Drug Response")
lines(dose, drugB, type = "b", pch = 17, lty = 2, col = "blue")
abline(h=c(30), lwd = 1.5, lty = 2, col = "gray")
library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = .5)
legend("topleft", inset = .05, title = "Drug Type", c("A", "B"),
       lty = c(1,2), pch = c(15,17), col = c("red", "blue"))
par(opar)

## 3.8 adding text
### Example 1
attach(mtcars)
plot(wt, mpg, 
     main = "Mileage vs. Car Weight", 
     xlab = "Weight", ylab = "Mileage",
     pch = 18, col = "blue")
text(wt, mpg, 
     row.names(mtcars),
     cex = .6, pos = 4, col = "red")
detach(mtcars)
### Example 2
opar <- par(no.readonly = T)
par(cex=1.5)
plot(1:7, 1:7, type = "n")
text(3, 3, "Example of default text")
text(4, 4, family = "mono", "Example of mono-spaced text")
text(5, 5, family = "serif", "Example of serif text")
par(opar)

## 3.9 Layout of graphs
### Example 1
attach(mtcars)
opar <- par(no.readonly = T)
par(mfrow = c(2, 2))
plot(wt, mpg, main = "Scatterplot of wt vs. mpg")
plot(wt, disp, main = "Scatterplot of wt vs. disp")
hist(wt, main = "Hisogram of wt")
boxplot(wt, main = "Boxplot of wt")
detach(mtcars)
### Example 2
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = T), 
       widths = c(3, 1), heights = c(1,2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

## 3.10
par <- par(no.readonly = T)
par(fig = c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg, 
     xlab = "Miles Per Gallon", 
     ylab = "Car Weight")
par(fig = c(0, 0.8, 0.55, 1), new = T)
boxplot(mtcars$wt, horizontal = T, axes = F)

par(fig = c(0.65, 1, 0, 0.8), new = T)
boxplot(mtcars$mpg, axes = F)
mtext("Enhanced Scatterplot", side = 3, outer = T, line = -3)
par(opar)
