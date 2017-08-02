yc <- read.csv("~/Documents/Learning/R/scripts/data/yellowcroaker.csv")
names(yc) <- c("time", "area", "long", "lat", "sex", "length", "weight")

yc <- yc[c(-2,-3,-4,-7)]
yc <- subset(yc, !is.na(yc$length))
yc <- within(yc, {
                    len_grp <- 0
                    len_grp[length <= 50] <- "<= 50"
                    len_grp[length > 50 & length <= 100] <- "<= 100"
                    len_grp[length > 100 & length <= 150] <- "<= 150"
                    len_grp[length > 150 & length <= 200] <- "<= 200"
                    len_grp[length > 200 & length <= 250] <- "<= 250"
                    })
library(sqldf)
yc_observe <- sqldf("select time, sex, len_grp 
                    from yc 
                    where sex like 'F%' or sex IN ('M', '-') 
                    order by time, sex")
yc_observe <- within(yc_observe, {
                    time <- substr(time,4,5)
                    })
opar <- par(no.readonly = T)
par(mfrow=c(3,1))
sex_time <- xtabs(~sex+time, data = yc_observe)
mosaic(sex_time)
sex_len_grp <- xtabs(~sex+len_grp, data = yc_observe)
mosaic(sex_len_grp)
time_len_grp <- xtabs(~time+len_grp, data = yc_observe)
mosaic(time_len_grp)
par <- opar
