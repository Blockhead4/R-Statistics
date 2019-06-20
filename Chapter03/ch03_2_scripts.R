# 3-2. 분포함수

# d ~ 분포 : 값이 주어졌을 때 그 확률
# p ~ 분포 : 값이 주어졌을 때 누적 확률
# q ~ 분포 : 누적확률이 주어졌을 때 그 값
# r ~ 분포 : 난수 생성

# 이항분포
n <- 6
p <- 1/3
x <- 0:n

# dbinom(x, size, prob) 
dbinom(2, size=n, prob=p)
dbinom(4, size=n, prob=p)
px <- dbinom(x, n, p)
plot(x, px, type="h", xlab="성공 횟수(x)", ylab="확률(P[X=x])", 
     main="B(6, 1/3)", col=rainbow(length(x)), lwd=30)
barplot(px, xlab="성공 횟수(x)", ylab="확률(P[X=x])", main="B(6, 1/3)")

# pbinom
pbinom(2, n, p)
pbinom(4, n, p)
pbinom(4, n, p) - pbinom(2, n, p)
px2 <- pbinom(x, n, p)
plot(x, px2, type="h", xlab="성공 횟수(x)", ylab="확률(P[X=x])",
     main="누적 B(6, 1/3)", col=rainbow(length(x)), lwd=30)

# qbinom
qbinom(0.1, n, p)
qbinom(0.5, n, p)

# rbinom
rbinom(10, n, p)

# R의 분포함수를 이용한 기댓값과 분산
n <- 6
p <- 1/3
x <- 0:n
px <- dbinom(x, n, p)
ex = sum(x * px)    # 기대값(Expectation Value)
ex2 = sum(x^2 * px)
vx <- ex2 - ex^2    # 분산(Variance)
vx

n * p               # 이항분포의 기댓값 : np
n * p * (1-p)       # 이항분포의 분산 : np(1-p), npq


# 정규분포
options(digits = 3)
mu <- 170
sigma <- 6
ll <- mu - 3*sigma    # lower limit
ul <- mu + 3*sigma    # upper limit

x <- seq(ll, ul, by=0.01)
nd <- dnorm(x, mean=mu, sd=sigma)
plot(x, nd, type="l", xlab="x", ylab="P[X=x]", 
     lwd=2, main="Normal Distribution ~ (170, 6^2)", col="red")

pnorm(mu, mean=mu, sd=sigma)
pnorm(158, mean=mu, sd=sigma)
pnorm(180, mu, sigma)

qnorm(0.25, mu, sigma)     # 1분위수
qnorm(0.5, mu, sigma)      # 평균값
qnorm(0.975, mu, sigma)    # 신뢰수준 95% 상한값

options(digits=5)
set.seed(5)
smp <- rnorm(400, mean=mu, sd=sigma)
c(mean(smp), sd(smp))
hist(smp, prob=T, main="N(170, 6^2) 으로부터 추출한 표본의 분포(n=400)", 
     xlab="", ylab="", col=rainbow(8), border="black")
lines(x, nd, lty=2, col="red")

# 정규분포의 특징 알아보기
options(digits=3)
mu <- 0
sigma <- 1
p0.05 <- qnorm(0.05, mu, sigma)       # 90% 신뢰수준 값
p0.025 <- qnorm(0.025, mu, sigma)     # 95% 신뢰수준 값
p0.005 <- qnorm(0.005, mu, sigma)     # 99% 신뢰수준 값
p0.05; p0.025; p0.005

pnorm(1.645, mu, sigma) - pnorm(-1.645, mu, sigma)
pnorm(1.96, mu, sigma) - pnorm(-1.96, mu, sigma)
pnorm(2.58, mu, sigma) - pnorm(-2.58, mu, sigma)

# p.143 그림 3-17
z <- seq(-3, 3, by=0.001)
z.p <- dnorm(z)
plot(z, z.p, axes=F, type="l", main="표준정규분포 (95%)", ylab="", ylim=c(-0.04, 0.4))
axis(1)

lines(c(-3, 3), c(0, 0))
points(-1.96, -0.02, pch=17, col="red")
text(-1.96, -0.035, "-1.96", col="red")
points(1.96, -0.02, pch=17, col="red")
text(1.96, -0.035, "1.96", col="red")

s <- seq(-1.96, 1.96, by=0.001)
s.z <- dnorm(s, mean=0, sd=1)
s <- c(-1.96, s, 1.96)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="red", density=10, angle=305)

