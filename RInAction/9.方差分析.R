# 9. 方差分析
## 9.1 单因素方差分析
# install.packages("multcomp")
library(multcomp)
attach(cholesterol)
table(trt)
aggregate(response, by=list(trt), FUN=mean)
aggregate(response, by=list(trt), FUN=sd)
fit <- aov(response ~ trt)
# F检验非常显著(p<0.0001)，说明五种 法的效果不同
summary(fit)
library(gplots)
plotmeans(response ~ trt, xlab = "Treatment", ylab = "Response",
          main = "Mean Plot\n with 95% CI")
detach(cholesterol)

### 9.1.1 多重比较
opar <- par(no.readonly = T)
par(las = 2) # 旋转轴标 
par(mar=c(5,8,4,2))
TukeyHSD(fit)
plot(TukeyHSD(fit))

# Tukey HSD检验
library(multcomp)
tuk <- glht(fit, linfct = mcp(trt="Tukey"))
# 有相同字 的组(用箱线图表示)说明均值差异不显著
plot(cld(tuk, level = .05), col = "lightgrey")

### 9.1.2 评估检验的假设条件
# 单因素方差分析中，我们假设因变量服从正态分布，各组方差相等
library(car)
qqPlot(lm(response ~ trt, data = cholesterol), 
        simulate = T, main = "Q-Q Plot", labels = F)
# 方差齐性检验（F检验）：是方差分析的重要前提，是方差可加性原则应用的一个条件
bartlett.test(response ~ trt, data = cholesterol)
# 方差齐性分析对离群点非常敏感
library(car)
# 当p>1时将产生NA,说明没有离群点
outlierTest(fit)

## 9.2 单因素协方差方差分析（ANCOVA）
data(litter, package = "multcomp")
attach(litter)
table(dose)
aggregate(weight, by=list(dose), FUN=mean)
fit <- aov(weight ~ gesttime + dose) # gesttime为协变量
summary(fit)
detach(litter)

# 去除协变量效应后的组均值
# install.packages("effects")
library(effects)
effect("dose", fit)

# 对用户定义的对照的多重比较
library(multcomp)
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose = contrast)))

### 9.2.1 评估检验的假设条件
library(multcomp)
fit2 <- aov(weight ~ gesttime * dose, data = litter)
summary(fit2)

### 9.2.2 结果可视化
# install.packages("HH")
library(HH)
ancova(weight ~ gesttime + dose, data = litter)

## 9.3 双因素方差分析
attach(ToothGrowth)
table(supp, dose)
aggregate(len, by=list(supp, dose), FUN = mean)
aggregate(len, by=list(supp, dose), FUN = sd)
dose <- factor(dose)
fit <- aov(len ~ supp * dose)
summary(fit)
ancova(len ~ supp * dose, data = ToothGrowth)
interaction.plot(dose, supp, len, type = "b",
                 col=c("red", "blue"), pch = c(16, 18),
                 main = "Interaction between Dose and Supplement Type")
detach(ToothGrowth)
library(HH)
interaction2wt(len ~ supp * dose)

## 9.4 重复测量方差分析
w1b1 <- subset(CO2, Treatment=='chilled')
fit <- aov(uptake ~ Type*conc + Error(Plant/(conc)), w1b1)
summary(fit)
opar <- par(no.readonly = T)
par(las = 2)
par(mar=c(10, 4, 4, 2))
with(w1b1, interaction.plot(conc, Type, uptake,
                            type = "b", col = c("red", "blue"), pch = c(16, 18),
                            main = "Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type*conc, data = w1b1, col = c("gold", "green"),
        main = "Chilled Quebec and Mississippi Plants",
        ylab = "Carbon dioxide uptake rate (umol/m^2 sec)")

## 9.5 多元方差分析
library(MASS)
attach(UScereal)
shelf <- factor(shelf)  
y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN=mean)
cov(y)
fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit) # 输出单变量结果
detach(UScereal)

### 9.5.1
### 单因素多元方差分析有两个前提假设，一个是多元正态性，一个是方差—协方差矩阵同质性
### QQ图展示卡方分布的分位数，横纵坐标分别是样本量与马氏距离平方值，如果点全部落在斜率为1、截距项为0的直线上，则表明数据服从多元正态分布
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y, center, cov)
coord <- qqplot(qchisq(ppoints(n), df=p), d, main = "Q-Q Plot Assessing Multivariate Normality",
                ylab = "Mahalanobis D2")
abline(a=0, b=1)
identify(coord$x, coord$y, labels = row.names(UScereal))

### find outliers
# install.packages("mvoutlier")
library(mvoutlier)
outliers <- aq.plot(y)
outliers

### 9.5.2
# 多元正态性或者方差—协方差均值假设都不满足，或者担心多元离群点，可以用稳健或非参数版本的MANOVA检验
# install.packages("rrcov")
library(rrcov)
Wilks.test(y, shelf, method="mcd")

