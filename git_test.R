# Chapter 04. Practice

# 1. Chisqure

# 1) 표본 크기의 변화에 따른 Chisqure 분포 <<< 중심극한정리
set.seed(9)
n <- 1000
chi.4.mean <- rep(NA, n)
chi.12.mean <- rep(NA, n)
chi.128.mean <- rep(NA, n)
chi.512.mean <- rep(NA, n)

for(i in 1:n) {
  chi.4.mean[i] <- mean(rchisq(4, df=1))
  chi.12.mean[i] <- mean(rchisq(12, df=1))
  chi.128.mean[i] <- mean(rchisq(128, df=1))
  chi.512.mean[i] <- mean(rchisq(512, df=1))
}

# 표본평균들의 평균과 표준편차
options(digits=4)
c(mean(chi.4.mean), sd(chi.4.mean))
c(mean(chi.12.mean), sd(chi.12.mean))
c(mean(chi.128.mean), sd(chi.128.mean))
c(mean(chi.512.mean), sd(chi.512.mean))

# Chi(df = k). mean: k, sd: sqrt(2*k/n)
par(mfrow=c(2,2))
hist(chi.4.mean, prob=TRUE, main="표본 크기: 4", col="orange", border="red")
x1 <- seq(min(chi.4.mean), max(chi.4.mean), length=1000)
y1 <- dnorm(x=x1, mean=1, sd=sqrt(2/4))
lines(x1, y1, lty=2, col="blue")

hist(chi.12.mean, prob=TRUE, main="표본 크기: 12", col="orange", border="red")
x2 <- seq(min(chi.12.mean), max(chi.12.mean), length=1000)
y2 <- dnorm(x=x2, mean=1, sd=sqrt(2/12))
lines(x2, y2, lty=2, col="blue")

hist(chi.128.mean, prob=TRUE, main="표본 크기: 128", col="orange", border="red")
x3 <- seq(min(chi.128.mean), max(chi.128.mean), length=1000)
y3 <- dnorm(x=x3, mean=1, sd=sqrt(2/128))
lines(x3, y3, lty=2, col="blue")

hist(chi.512.mean, prob=TRUE, main="표본 크기: 512", col="orange", border="red")
x4 <- seq(min(chi.512.mean), max(chi.512.mean), length=1000)
y4 <- dnorm(x=x4, mean=1, sd=sqrt(2/512))
lines(x4, y4, lty=2, col="blue")


# 2) 자유도의 크기에 따른 Chisqure 분포 차이
set.seed(9)
n <- 1000
chi.d1.mean <- rep(NA, n)
chi.d4.mean <- rep(NA, n)
chi.d16.mean <- rep(NA, n)
chi.d32.mean <- rep(NA, n)

rchisq(4, df=1)
for(i in 1:n) {
  chi.d1.mean[i] <- mean(rchisq(4, df=1))
  chi.d4.mean[i] <- mean(rchisq(4, df=4))
  chi.d16.mean[i] <- mean(rchisq(4, df=16))
  chi.d32.mean[i] <- mean(rchisq(4, df=32))
}

# 표본평균들의 평균과 표준편차
options(digits=4)
c(mean(chi.d1.mean), sd(chi.d1.mean))
c(mean(chi.d4.mean), sd(chi.d4.mean))
c(mean(chi.d16.mean), sd(chi.d16.mean))
c(mean(chi.d32.mean), sd(chi.d32.mean))

# Chi(df = k). mean: k, sd: sqrt(2*k/n)
par(mfrow=c(2,2))
hist(chi.d1.mean, prob=TRUE, main="자유도 크기: 1", col="orange", border="red")
x1 <- seq(min(chi.d1.mean), max(chi.d1.mean), length=1000)
y1 <- dnorm(x=x1, mean=1, sd=sqrt(2/4))
lines(x1, y1, lty=2, col="blue")

hist(chi.d4.mean, prob=TRUE, main="자유도 크기: 4", col="orange", border="red")
x2 <- seq(min(chi.d4.mean), max(chi.d4.mean), length=1000)
y2 <- dnorm(x=x2, mean=4, sd=sqrt(8/4))
lines(x2, y2, lty=2, col="blue")

hist(chi.d16.mean, prob=TRUE, main="자유도 크기: 16", col="orange", border="red")
x3 <- seq(min(chi.d16.mean), max(chi.d16.mean), length=1000)
y3 <- dnorm(x=x3, mean=16, sd=sqrt(32/4))
lines(x3, y3, lty=2, col="blue")

hist(chi.d32.mean, prob=TRUE, main="자유도 크기: 32", col="orange", border="red")
x4 <- seq(min(chi.d32.mean), max(chi.d32.mean), length=1000)
y4 <- dnorm(x=x4, mean=32, sd=sqrt(64/4))
lines(x4, y4, lty=2, col="blue")


# 2. T 
set.seed(9)
n <- 1000
t.d4.mean <- rep(NA, n)
t.d16.mean <- rep(NA, n)
t.d32.mean <- rep(NA, n)
t.d64.mean <- rep(NA, n)

for(i in 1:n) {
  t.d4.mean[i] <- mean(rt(4, df=4))
  t.d16.mean[i] <- mean(rt(4, df=16))
  t.d32.mean[i] <- mean(rt(4, df=32))
  t.d64.mean[i] <- mean(rt(4, df=64))
}

# 표본평균들의 평균과 표준편차
options(digits=4)
c(mean(t.d4.mean), sd(t.d4.mean))
c(mean(t.d16.mean), sd(t.d16.mean))
c(mean(t.d32.mean), sd(t.d32.mean))
c(mean(t.d64.mean), sd(t.d64.mean))

# T(df = k). mean: 0, sd: sqrt((k/k-2)/n), k>2
par(mfrow=c(2,2))
hist(t.d4.mean, prob=TRUE, ylim=c(0,0.6), main="자유도 크기: 4", col="orange", border="red")
x1 <- seq(min(t.d4.mean), max(t.d4.mean), length=1000)
y1 <- dnorm(x=x1, mean=0, sd=sqrt(4/2/4))
lines(x1, y1, lty=2, col="blue")

hist(t.d16.mean, prob=TRUE, ylim=c(0,0.8), main="자유도 크기: 16", col="orange", border="red")
x2 <- seq(min(t.d16.mean), max(t.d16.mean), length=1000)
y2 <- dnorm(x=x2, mean=0, sd=sqrt(16/14/4))
lines(x2, y2, lty=2, col="blue")

hist(t.d32.mean, prob=TRUE, ylim=c(0,0.8), main="자유도 크기: 32", col="orange", border="red")
x3 <- seq(min(t.d32.mean), max(t.d32.mean), length=1000)
y3 <- dnorm(x=x3, mean=0, sd=sqrt(32/30/4))
lines(x3, y3, lty=2, col="blue")

hist(t.d64.mean, prob=TRUE, ylim=c(0,0.8), main="자유도 크기: 64", col="orange", border="red")
x4 <- seq(min(t.d64.mean), max(t.d64.mean), length=1000)
y4 <- dnorm(x=x4, mean=0, sd=sqrt(64/62/4))
lines(x4, y4, lty=2, col="blue")



# 3. F
set.seed(9)
n <- 1000
f.d5_d6.mean <- rep(NA, n)
f.d6_d10.mean <- rep(NA, n)
f.d10_d20.mean <- rep(NA, n)
f.d40_d45.mean <- rep(NA, n)

for(i in 1:n) {
  f.d5_d6.mean[i] <- mean(rf(4, df1=5, df2=6))
  f.d6_d10.mean[i] <- mean(rf(4, df1=6, df2=10))
  f.d10_d20.mean[i] <- mean(rf(4, df1=10, df2=20))
  f.d40_d45.mean[i] <- mean(rf(4, df1=40, df2=45))
}

# 표본평균들의 평균과 표준편차
options(digits=4)
c(mean(f.d5_d6.mean), sd(f.d5_d6.mean))
c(mean(f.d6_d10.mean), sd(f.d6_d10.mean))
c(mean(f.d10_d20.mean), sd(f.d10_d20.mean))
c(mean(f.d40_d45.mean), sd(f.d40_d45.mean))

# F(df1 = k, df2 = l). mean: l/l-2, sd: (2*(l^2)*(k+l-2))/(k*((l-2)^2)*(l-4)), l>5
num1 <- c(5,6,10,40)
num2 <- c(6,10,20,45)

fm <- c()
fsd <- c()
for(i in 1:4) {
  fm <- c(fm, num2[i]/(num2[i]-2))
  fsd <- c(fsd, sqrt(((2*(num2[i]^2)*(num1[i]+num2[i]-2))/(num1[i]*((num2[i]-2)^2)*(num2[i]-4)))/4))
}

par(mfrow=c(2,2))
hist(f.d5_d6.mean, prob=TRUE, main="자유도 크기: 5 - 6", col="orange", border="red")
x1 <- seq(min(f.d5_d6.mean), max(f.d5_d6.mean), length=1000)
y1 <- dnorm(x=x1, mean=fm[1], sd=fsd[1])
lines(x1, y1, lty=2, col="blue")

hist(f.d6_d10.mean, prob=TRUE, main="자유도 크기: 6 - 10", col="orange", border="red")
x2 <- seq(min(f.d6_d10.mean), max(f.d6_d10.mean), length=1000)
y2 <- dnorm(x=x2, mean=fm[2], sd=fsd[2])
lines(x2, y2, lty=2, col="blue")

hist(f.d10_d20.mean, prob=TRUE, main="자유도 크기: 10 - 20", col="orange", border="red")
x3 <- seq(min(f.d10_d20.mean), max(f.d10_d20.mean), length=1000)
y3 <- dnorm(x=x3, mean=fm[3], sd=fsd[3])
lines(x3, y3, lty=2, col="blue")

hist(f.d40_d45.mean, prob=TRUE, main="자유도 크기: 40 - 45", col="orange", border="red")
x4 <- seq(min(f.d40_d45.mean), max(f.d40_d45.mean), length=1000)
y4 <- dnorm(x=x4, mean=fm[4], sd=fsd[4])
lines(x4, y4, lty=2, col="blue")

