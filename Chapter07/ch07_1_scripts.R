# 7장. 여러 모집단의 평균 비교 검정
# 7-1. 모집단이 두 개

# 1) 남아 신생아와 여아 신생아의 몸무게
setwd("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter07")
data <- read.table('data/chapter7.txt', header = T)
head(data)

boy <- subset(data, gender ==1)
girl <- subset(data, gender ==2)

#정규성 테스트
shapiro.test(boy$weight)
qqnorm(boy$weight)
qqline(boy$weight)

shapiro.test(girl$weight)
qqnorm(girl$weight)
qqline(girl$weight)

iriss <- subset(iris, Species =='setosa')
shapiro.test(iriss$Sepal.Length) # p-value > 0.05, 정규성 있음
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)

shapiro.test(iriss$Petal.Width) # p-value < 0.05, 정규성 없음
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)

# 등분산성 테스트
var.test(data$weight ~ data$gender)

# 2-sample T test
t.test(data$weight ~ data$gender, mu=0, alternative="less", var.equal=T)

# 2) 식욕부진증 치료요법의 효과 검정
install.packages("PairedData")
library(PairedData)
data(Anorexia)
data <- Anorexia
head(data)

install.packages("psych")
library(psych)
summary(data)
describe(data)

n <- length(data$Prior - data$Post); n
m <- mean(data$Prior - data$Post); m
s <- sd(data$Prior - data$Post); s
t.t <- m/(s/sqrt(n)); t.t
alpha <- 0.05
qt(alpha, df=16)
pt(t.t, df=16)     # 검정통계량으로부터 구한 유의확률

t.test(data$Prior, data$Post, paired=T, alternative ="less")
