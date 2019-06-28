# 9장. 상관과 회귀
# 9-1. 상관계수
# 1) 아버지와 아들 키의 공분산과 상관계수
setwd("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter09")
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header=T, stringsAsFactors = F)
str(hf)
head(hf)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender == "M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father - f.mean) * (hf.son$Height - s.mean))
(cov.xy <- cov.num / (nrow(hf.son) -1))

(r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)))
# R 함수를 이용한 상관계수
cov(hf.son$Father, hf.son$Height)       # 공분산
cor(hf.son$Father, hf.son$Height)       # 상관계수
