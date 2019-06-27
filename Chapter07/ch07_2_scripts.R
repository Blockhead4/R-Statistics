# 7-2. 모집단이 세 개 이상
setwd("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter07")

# 지역규모별 나이 평균에 차이가 있는가?
ad <- read.csv("data/age.data.csv")
str(ad)

ad$score <- ifelse(ad$score == 99, NA, ad$score)
ad$scale <- factor(ad$scale)
ad$sex <- factor(ad$sex)

y1 <- ad$age[ad$scale == "1"]
y2 <- ad$age[ad$scale == "2"]
y3 <- ad$age[ad$scale == "3"]

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum((y1 - y1.mean)^2)
sse.2 <- sum((y2 - y2.mean)^2)
sse.3 <- sum((y3 - y3.mean)^2)
sse <- sse.1 + sse.2 + sse.3
dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1)

y.mean <- mean(ad$age)

sst.1 <- length(y1) * sum((y1.mean - y.mean)^2)
sst.2 <- length(y2) * sum((y2.mean - y.mean)^2)
sst.3 <- length(y3) * sum((y3.mean - y.mean)^2)
sst <- sst.1 + sst.2 + sst.3
dft <- length(levels(ad$scale)) - 1

tsq <- sum((ad$age - y.mean)^2)

mst <- sst/dft
mse <- sse/dfe
f.t <- mst/mse

alpha <- 0.05
tol <- qf(1-alpha, 2, 147)

p.value <- 1 - pf(f.t, 2, 147)

# 그래프 
x <- seq(0, 4, by=0.01)
yf <- df(x, 2, 147)
par(mar=c(2,1,1,1))
plot(x, yf, type="l", ylim=c(-0.1, 1), xlab="", ylab="", axes=F)
abline(h=0)
tol.r <- round(tol, 2)
polygon(c(tol.r, x[x>=tol.r], 4), c(0, yf[x>=tol.r], 0), col="red")
arrows(tol, 0.3, tol, 0.08, length=0.1)
text(tol, 0.32, paste("P(F(2, 147) > ", round(tol,2), ")", sep=""))

# R을 통한 p-value 구하기
ow <- lm(age~scale, data=ad)
ow
anova(ow)

oneway.test(age~scale, data=ad, var.equal = T)
