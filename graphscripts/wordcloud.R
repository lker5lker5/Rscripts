library(rJava)
library(Rwordseg)
#install.packages("wordcloud")
segmentCN("/Users/vinson/Documents/Learning/R/scripts/data/2017渔业渔政工作要点.txt")
installDict("/Users/vinson/Documents/Learning/R/scripts/data/governmentcliche.scel", "sougou")
listDict()

# == 读入数据  
passage = read.csv("/Users/vinson/Documents/Learning/R/scripts/data/2017渔业渔政工作要点.segment.txt",sep=" ",header=F,fileEncoding="UTF-8")

# == 文本预处理   
res = passage[passage != " "]
#剔除URL  
res=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",res);   
#剔除特殊词  
res=gsub(pattern="[我|你|的|了|是|一|二|三|四|五|六|七|八|九|十|而|以|和]","",res);

# 分词+频数统计
words = unlist(lapply(X=res, FUN = segmentCN))
word = lapply(X=words, FUN = strsplit, " ")
v = table(unlist(word))
# 降序排序  
v=rev(sort(v));   
d=data.frame(word=names(v), freq=v); 
# 过滤掉1个字和词频小于100的记录  
names(d)[1:3] <- c("word","word-freq","freq")
res_table = subset(d, nchar(as.character(word)) >= 2 & freq >= 20) 
res_table = res_table[-2]
#install.packages("devtools")
library(devtools)
#install.packages("wordcloud2")
library(wordcloud2)
letterCloud(res_table,word = "R", color = "random-light",
            backgroundColor = "gray",size = 0.3)
# In R, type
## .libPaths()
## go to this folder "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
## find the "example" folder, and drag the customized graph (in black & white) to this folder
figPath = system.file("examples/fish.jpg", package = "wordcloud2")
wordcloud2(res_table, figPath = figPath, size = 1.5, color = "skyblue")
legend("bottomright", legend = "@Vinson", bty = "n", text.col = "gray70")
