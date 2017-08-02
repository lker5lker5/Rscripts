#install.packages("vcd")
library(vcd)

mytable <- xtabs(~Treatment+Improved, data=Arthritis)
