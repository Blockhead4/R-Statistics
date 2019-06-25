# 6장. 가설검정
# 6-1. 가설검정
setwd("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter06")
data <- read.csv("data/2010_6차_남자.csv")
str(data)

tmp <- subset(data, data$나이==7)
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height

mean(height)
sd(height)
t.test(height, mu=1220)
