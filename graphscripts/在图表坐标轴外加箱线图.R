opar <- par(no.readonly = TRUE)
par(fig=c(0,0.8,0,0.8))
with(mtcars, 
     plot(wt, mpg, xlab="Miles Per Gallon", ylab="Car Weight"))

#在上方添加箱线图
par(fig=c(0,0.8,0.55,1), new = TRUE)
boxplot(mtcars$wt, horizontal = TRUE, axes = FALSE)

#在youce添加箱线图
par(fig=c(0.65, 1, 0, 0.8), new = TRUE)
boxplot(mtcars$mpg, axes = FALSE)
mtext("Enhanced Scatterplot", side = 3, outer = TRUE, line = -3)
par(opar)