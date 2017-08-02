sprdata <- read.csv("~/Personal/MarineFishery/papers/chart/data/sprEnvFactor.csv", header = T, 
                    sep = ",", row.names = "position")
# 对不同单位的数据进行标准化
library(ggplot2)
sprdatascaled <- as.data.frame(lapply(sprdata, ggplot2:::rescale01))
# 将数据集中的每个列的相关系数统计出来并保存在一个corr的参数中
corr <- cor(sprdatascaled)
# install.packages("corrplot")
library(corrplot)
par(mar = c(4, 5, 5, 4) + 0.1)
opar <- par(no.readonly = T)
par(family = "STKaiti")
corrplot(corr = corr, add = T, method = "color", order = "AOE", addCoef.col = "whitesmoke",
         outline = T, diag = F, title = "某生物发生量和环境影子的因子的关系")
par <- opar
