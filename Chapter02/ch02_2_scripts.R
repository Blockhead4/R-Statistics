# 모수와 통계량
# 라니카페

setwd("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter02")
ranicafe <- read.csv("data/cafedata.csv", stringsAsFactors = F)

str(ranicafe)
head(ranicafe)
summary(ranicafe)
dim(ranicafe)

# 최소값, 최대값
ranicafe$Coffees <- as.numeric(ranicafe$Coffees)
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1]     # 최소값
sort(ranicafe$Coffees, decreasing = T)[1]     # 최대값
min(ranicafe$Coffees, na.rm = T)
max(ranicafe$Coffees, na.rm = T)

# 최빈값
# 줄기-잎 그림
hist(ranicafe$Coffees)
stem(ranicafe$Coffees)    # 최빈값

# 평균, 중앙값
rc <- ranicafe$Coffees
weight <- 1 / length(rc)-1     # NA 개수를 빼주어야 함
weight <- 1 / length(subset(rc, !is.na(rc)))
weight <- 1 / length(rc[!is.na(rc)==T])
weight
sum(rc, na.rm = T) * weight    # 평균
mean(rc, na.rm = T)

rc[rc == max(rc, na.rm = T)] <-480
mean(rc, na.rm = T)
boxplot(rc[rc<470])

median.idx <- (length(subset(rc, !is.na(rc))) + 1) / 2   # 중앙값
median.idx
sort(rc)[median.idx]
median(rc, na.rm = T)

# 표쥰편차와 사분위수 범위
height <- c(164, 166, 168, 170, 172, 174, 176)
height.m <- mean(height)
height.dev <- height - height.m    # 편차
sum(height.dev)
height.dev2 <- height.dev^2
sum(height.dev2)
mean(height.dev2)    # 분산

# 표준편차 구하기
sqrt(mean(height.dev2))

# 분산과 표준편차 구하기
var(height)
sd(height)

# 사분위수 구하기
quantile(rc, na.rm=T)
qs <- quantile(rc, na.rm=T)
qs 
qs[4] - qs[2]        # 3분위수 - 1분위수 -> IQR (Inter Quantile Range)
IQR(rc, na.rm=T)     # IQR : 사분위수 범위
bp <- boxplot(rc, main="커피 판매량에 대한 상자도표", axes=F)

# 이상치(Outlier)
boxplot(cars$dist)
qs <- quantile(cars$dist)
qs
iqr <- qs[4] - qs[2]
iqr
upper_limit <- qs[4] + (1.5 * iqr)
lower_limit <- qs[2] - (1.5 * iqr)
lower_limit; upper_limit
cars$dist[cars$dist > upper_limit]    # 이상치
cars$dist[cars$dist < lower_limit]    # 이상치
