# 1. linear regression
## 1.1 simple linear regression
fit <- lm(weight ~ height, data = women)
summary(fit)
plot(women$height, women$weight,
     xlab = "Height (inches)",
     ylab = "Weight (pounds)")
abline(fit)

fit2 <- lm(weight ~ height + I(height^2), data = women)
summary(fit2)
lines(women$height, fitted(fit2))

# linear regression and fitted regression
library(car)
scatterplot(weight ~ height, data = women,
            spread = F, smoother.args=list(lty=2), pch = 19,
            main = "Women Age 30 - 39",
            xlab = "Height (inches)",
            ylab = "Wright (lbs.)")

## 1.2 Multiple linear regression
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
### a. check correlation between variables
cor(states, method = "spearman")
library(car)
### 当预测变量不止一个时，，回归系数的含义为，一个预测变量增加一个单位，其他预测变量保持不变时，
### 因变量要增加的数量。例如，文盲率的回归系数为4.14，表示控制人口、收入、和温度不变时，文盲率
### 上升1%，谋杀率上升4.14%，它的系数p<0.001的水平下显著不为0.相反，frost的系数没有显著不为0，
### 表明当控制其他变量不变时，Frost与Murder不呈线性关系。总体来看，所有的预测变量解释了各州谋杀
### 率57%的方差。
scatterplotMatrix(states, spread = F, smoother.args = list(lty=2), 
                  main = "Scatter Plot Matrix")
fitMul <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fitMul)

## 1.3 Multi linear regression with Interaction terms
### Pr(> |t|)栏中，马力与车重的交互项是显著的，说明响应变量与其中一个预测变量的关系依赖于另外一
### 个预测变量的水平，从而说明每加仑汽油行驶英里数与汽车马力的关系依车重不同而不同
### mpg = 49.81 - 0.12hp - 8.22wt + 0.03hp * wt
fitMulInter <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fitMulInter)

# 2. Regression diagnostics
## 回归诊断技术提供了评价模型适用性
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
fitMul <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
### 结果表明， 文盲率改变1%， 谋杀率就在95%的置信区间[2.38,5.90]中变化， 因为Frost的置信区间包含0
### 可知，当其他变量不变时，温度的改变与谋杀率无关。
confint(fitMul)
## 2.1 Normal Method
opar <- par(no.readonly = T)
par(mfrow = c(2,2))
plot(fit)
## 2.2 Improved Method
### 2.2.1 Normal Distribution Test - qqplot
library(car)
par <- par(opar)
qqPlot(fitMul, labels = row.names(states), id.method = "identify", simulate = T, main = "Q-Q plot")
states["Nevada",]
fitted(fitMul)["Nevada"]
residuals(fitMul)["Nevada"]
rstudent(fitMul)["Nevada"] # rstudent: to make data to be t distribution

### draw student's residual plot
residplot <- function(fit, nbreaks = 10) {
  t <- rstudent(fit)
  hist(t, breaks = nbreaks, freq = F,
       xlab = "Studentized Residual",
       main = "Distribution of Errors")
  rug(jitter(t), col = "brown")
  curve(dnorm(x, mean = mean(t), sd = sd(t)),
              add = T, col = "blue", lwd = 2)
  lines(density(t)$x, density(t)$y,
        col = "red", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Normal Curve", "Kernel Density Curve"),
         lty = 1:2, col = c("blue", "red"), cex = .7)
}
residplot(fitMul)

### 2.2.2 patial residual plot
library(car)
#### 如果图形存在非线性，说明对预测的函数形式建模不充分，需要添加一些曲线成分，比如多项式，
#### 或对一个或多个变量进行变换如log(x)
crPlots(fitMul)

### 2.2.3 同方差性
library(car)
#### ncvTest(): 生成一个计分检验，零假设误差方差不变，备择假设为误差随着拟合值水平的变化而变化。
####            若检验显著，则说明存在异方差性(误差方差不恒定)
#### spreadLevelPlot(): 创建一个最佳拟合曲线三点图，展示标准化残差绝对值与拟合值的关系
ncvTest(fitMul)  # 计分检验不显著(p=0.19)，说明满足方差不变假设
spreadLevelPlot(fitMul) # 点在水平的最佳拟合曲线周围呈水平随机分布则满足方差不变假设
                        # suggested power transformation 表示p次幂变化，非恒定的误差方差会平稳。
                        # 如果建议转化是0.5，用√x代替x；如果是0，则使用对数变换；
                        # 接近1则说明异方差不明显

## 2.3 线性模型的综合验证
#install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fitMul)
summary(gvmodel) # 给模型假设提供了一个单独的综合检验

## 2.4 多重共线性: 导致模型参数的置信区间过大，使单个系数解释起来困难
### 解决方法：删除不重要的自变量，改变解释变量的形式
library(car)
vif(fitMul)
sqrt(vif(fitMul)) > 2 # √vif > 2 意味着存在多重共线性

# 3. 异常值观测
## 3.1 离群点的统计
library(car)
outlierTest(fitMul) # 删除离群点，然后再检验是否还有其他离群点的存在

## 3.2 高杠杆值观测点
###    即是与其他预测变量有关的离群点,由许多异常的预测 变量值组合起来的，与响应变量值没有关系
# 高杠杆值的观测点可通过帽子统计量(hat statistic)判断。
# 对于一个给定的数据集，帽子均 值为p/n, 其中p 是模型估计的参数数目(包含截距项)，n 是样本量。
# 一般来说，若观测点的帽子值大于帽子均值的2或3倍，即可以认定为高杠杆值点。
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fitMul)

## 3.3 强影响点
###    Cook’s D值大于4/(n-k- 1)，则表明它是强影响点，其中n 为样本量大小，k 是预测变量数目
cutoff <- 4/(nrow(states)-length(fitMul$coefficients)-2)
plot(fitMul, which = 4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")
### Improved 
library(car)
#### 纵坐标超过+2或小于2的州可被认为是离群点，水平轴超过0.2或0.3 的州有高杠杆值(通常为预测值的组合)
#### 圆圈大小与影响成比例，圆圈很大的点可能是对模型参数的估计造成的不成比例影响的强影响点
influencePlot(fitMul, id.method = "identify", main = "Influence Plot",
              sub = "Circle size is proportional to Cook's distance")

## 3.4 改进措施
### 3.4.1 删除观测点
#### 异常点可能是最有趣的东西。发掘为何该观测点不同于其他点，有助于你更深刻地理解研究的主题，
#### 或者发现其他你可能没有想过的问题

### 3.4.2 变量变换
#### 将X变换为1/Y2 1/Y 1/√Y log(Y) √Y 无 Y2
#### 若X是比例数，通常使用logit变换[ln (Y/1Y )]libra
library(car)
summary(powerTransform(states$Murder))
    # 结果表明，可以用Murder^0.6来正态化变量Murder。由于0.6很接近0.5，可以尝试用平方根变换来
    # 提高模型正态性的符合程度。但在本例中，λ= 1的假设也无法拒绝(p=0.145)
    # 因此没有强有力的证据表明本例需要变量变换
#### 当违反了线性假设时，对预测变量进行变换常常会比较有用
library(car)
boxTidwell(Murder~Population+Illiteracy, data = states)
    # 结果显示，使用变换Population^0.87和Illiteracy^1.36能够大大改善线性关系。
    # 但是对Population (p=0.75)和Illiteracy(p=0.54)的计分检验又表明变量并不需要变换
    # 当数据满足了方差不变性，则不需要进行变量变换

### 3.4.3 增删变量
#### 删除存在多重共线性的变量

## 3.5 选择“最佳”的回归模型
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)

# anova方法: 需要嵌套模型,此处模型1嵌套在模型2中
anova(fit2, fit1) 
    # 由于检验不显著(p=0.994)，因此不需要将这两个变量添加到线性模型中，可以将它们从模型中删除
# AIC: AIC值越小的模型要优先选择，它说明模型用较少的参数 获得了足够的拟合度
AIC(fit1, fit2)

### 3.5.1 变量选择
#### 3.5.1.1 逐步回归: 模型会一次添加或者删除一个变量，直到达到某个判停准则为止 
# backward stepwise
library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
# WARNING: 逐步回归法其实存在争议，虽然它可能会找到一个好的模型
        ## 但是不能保证模型就是最佳模型，因为不是每一个可能的模型都被评价了
stepAIC(fit1, direction = "backward")

#### 3.5.1.2 全子集回归
# install.packages("leaps")
library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = states, nbest = 4)
plot(leaps, scale = "adjr2")

library(car)
subsets(leaps, statistic = "cp", main = "Cp Plot for All Subsets Regression")
abline(1, 1, lty=2, col="red")

## 3.6 深层次分析
### 3.6.1 交叉验证
#### 将一定比例的数据挑选出来作为训练样本，另外的样本作保留样本，先在训练样本上获取回归方程，
#### 然后在保留样本上做预测
# k 重交叉验证
install.packages("bootstrap")
shrinkage <- function(fit, k=10) {
  require(bootstrap)
  
  theta.fit <- function(x,y) {lsfit(x,y)}
  theta.predict <- function(fit,x) {cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square = ", r2, "\n")
  cat(k, "Fold Cross-Validated R-square = ", r2cv, "\n")
  cat("Change = ", r2-r2cv, "\n")
}
# 可以用交叉验证来挑选变量。例如，含两个预测变量 (Population和Illiteracy)的模型
# Changede的值减少得越少，预测则越精确
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data = states)
shrinkage(fit)

##  3.7 相对重要性
# 此处可以看到，当其他因素不变时，文盲率一个标准差的变化将增加0.68个标准差的谋杀率。 
# 根据标准化的回归系数，我们可认为Illiteracy是最重要的预测变量，而Frost是最不重要的
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data = zstates)
coef(zfit)

### 3.7.1 相对权重(relative weight)
relweight <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import), names.arg = lbls,
          ylab = "% of R-Square",
          xlab = "Predictor Variables",
          main = "Relative Importance of Predictor Variables",
          sub = paste("R-Square = ", round(rsquare, digits = 3)),
          ...)
  return(import)
}
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
# Illiteracy解释了 59%的R平方，Frost解释了20.79%。根据相对权重法，Illiteracy有最大的相对重要性，
# 余下依次 是Frost、Population和Income
relweight(fit, col = "lightgray")
