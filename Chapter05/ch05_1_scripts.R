# 5장. 추정 (Estimation)
# 5-1. 점추정

# 1) 유효성
par(mfrow=c(1,1))
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
y.1 <- dnorm(x, sd=sqrt(1/3))
y.2 <- dnorm(x, sd=sqrt(7/18))
pnorm(0.1, sd=sqrt(1/3)) - pnorm(-0.1, sd=sqrt(1/3))
pnorm(0.1, sd=sqrt(7/18)) - pnorm(-0.1, sd=sqrt(7/18))
plot(x, y, type="l", ylim=c(0, 0.8), axes=F, ylab="", lwd=3, col="yellow")
lines(x, y.1, col="red", lwd=3)
lines(x, y.2, col="green", lty=2, lwd=3)
axis(1)

# 2) 유효성 모의실험
options(digits=3)
set.seed(1)
# Y2bar 을 구하기 위한 함수
mean.seq <- function(x) {
  n <- length(x)
  sum <- 0
  n2 <- 0
  for(i in 1:n) {
    newx <- i * x[i]
    sum <- sum + newx
    n2 <- n2 + i
  }
  return(sum/n2)
}

y1 <- rep(NA, 1000)
y2 <- rep(NA, 1000)

for(i in 1:1000) {
  smp <- rnorm(3)
  y1[i] <- mean(smp)
  y2[i] <- mean.seq(smp)
}

n1 <- length(y1[(y1 > -0.1) & (y1 < 0.1)])
n2 <- length(y2[(y2 > -0.1) & (y2 < 0.1)])
data.frame(mean=mean(y1), var=var(y1), n=n1)
data.frame(mean=mean(y2), var=var(y2), n=n2)

par(mfrow=c(1,2))
hist(y1, probability = T, xlim=c(-2, 2), ylim=c(0, 0.65),
     main="(x1+x2+x3)/3", xlab="", col="orange", border="red")
hist(y2, probability = T, xlim=c(-2, 2), ylim=c(0, 0.65), 
     main="(1*x1+2*x2+3*x3)/6", xlab="", col="orange", border="red")

# 3) 모비율에 대한 점추정
library(prob)
n <- 3
smps.all <- rolldie(n)
str(smps.all)
head(smps.all, n=3)

is.even <- function(x) return(!x%%2)
var.p <- function(x) {
  return(sum((x-mean(x)^2)/length(x)))
}
p.even <- function(x, s.size=3) {
  return(sum(is.even(x))/s.size)
}

phat <- apply(smps.all, 1, p.even)

mean(phat)
(p.p <- 0.5)
var.p(phat)
(p.p*(1-p.p)/3)
sqrt(var.p(phat))