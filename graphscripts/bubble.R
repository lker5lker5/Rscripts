yc <- read.csv("~/Documents/Learning/R/scripts/data/yellowcroaker.csv", stringsAsFactors=FALSE)
names(yc) <- c("time", "area", "long", "lat", "sex", "length", "weight")
yc <- yc[c(-2,-3,-4,-7)]

yc <- within(yc, {
  len_grp <- 0
  len_grp[length <= 50] <- 50
  len_grp[length > 50 & length <= 100] <- 100
  len_grp[length > 100 & length <= 150] <- 150
  len_grp[length > 150 & length <= 200] <- 200
  len_grp[length > 200 & length <= 250] <- 250
})

library(sqldf)
yc_observe <- sqldf("select time, sex, length, len_grp 
                    from yc 
                    where sex like 'F%' or sex IN ('M', '-') 
                          and length is not null
                    order by time, sex")

sex_labels <- unique(yc_observe$sex)
time_labels <- unique(yc_observe$time)
yc_observe <- within(yc_observe, {
  sex[sex == "-"] <- 1
  sex[sex == "F2"] <- 2
  sex[sex == "F3"] <- 3
  sex[sex == "F4"] <- 4
  sex[sex == "F5"] <- 5
  sex[sex == "F62"] <- 6
  sex[sex == "M"] <- 7
  time[time == "16.09"] <- 1
  time[time == "16.11"] <- 2
  time[time == "16.12"] <- 3
  time[time == "17.01"] <- 4
  time[time == "17.03"] <- 5
})

opar <- par(no.readonly =T)
par(family="STKaiti")
library(ggplot2)
attach(yc_observe)
#r <- sqrt(len_grp/pi)
#symbols(sex, time, circles = r, inches = 0.30,
#        fg = "white", bg = "gray",
#        main = "调查时间与小黄鱼的性别、体长的关系",
#        xlab = "小黄鱼性别",
#        ylab = "调查时间",
#        ylim = c(1,7), xlim = c(1,5),
#        xaxt = "n", yaxt = "n",
#        xaxs = "i", yaxs = "i" # intersection at 0
#        )
#axis(1, at = seq(0,5), labels = append(time_labels,0,0), las = 0)
#axis(2, at = seq(0,7), labels = append(sex_labels,0,0), las = 2)
#text(time, sex, rownames(len_grp), cex=0.6, col = "black")
# 画图  
p = ggplot(yc_observe, aes(time, sex))  
p = p + geom_point()  
# 改变点的大小  
#p = p + geom_point(aes(size=len_grp))  
# 四维数据的展示  
pbubble = p + geom_point(aes(size=len_grp,color=length))  
# 自定义渐变颜色  
pbubble = pbubble+ scale_colour_gradient(low="gray",high="blue")  
# pbubble = pbubble + scale_color_grey(end=0.5)
# 绘制pathway富集散点图  
pr = pbubble + labs(color=expression(length),size="小黄鱼体长组",
                    x="调查时间",y="性别",title="调查时间与小黄鱼的性别、体长的关系") +
     scale_x_continuous(breaks = unique(time), labels = as.character(time_labels)) +
     scale_y_discrete(breaks = unique(sex), labels = sex_labels)
# 改变图片的样式（主题）  
pr = pr + theme_bw() + theme(text=element_text(family="STKaiti",size=14))
pr
detach(yc_observe)
