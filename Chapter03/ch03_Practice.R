# 이항분포와 정규분포 연습 문제
library(prob)
options(digits=3)

# R Practice - 이항분포

# 1. 다음의 문제가 베르누이 시행인지 판단하시오.

# 1) 영화관에서 줄을 기다리는 시간을 측정한다.
#    Answer : X

# 2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다.
#    Answer : O

# 3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다.
#    Answer : X

# 4) 주사위를 한 번 던졌을 때, 숫자 2가 나오는지를 체크한다.
#    Answer : O

# 2. 한 축구 선수가 페널티킥을 차면 5번 중 4번을 성공한다고 한다.
# 이 선수가 10번의 페널티킥을 차서 7번 성공할 확률을 구하시오.
n <- 10
p <- 4/5
dbinom(7, n, p)

# 3. A라는 회사는 스마트폰의 한 부품을 만드는 회사로, 이 A사의 불량률은 5%로 알려져 있다.
# 이 회사의 제품 20개를 조사했을 때, 불량이 2개 이하로 나올 확률을 구하시오.
n <- 20
p <- 5/100
pbinom(2, n, p)

# 4. 어떤 희귀 바이러스에 감염되었을 때, 회복할 수 있는 치료율은 20%라고 한다. 
# 이 바이러스에 감염된 환자 20명을 치료했을 때, 적어도 2명 이상은 회복될 확률을 구하시오.
n <- 20
p <- 20/100
1-pbinom(1, n, p)

# 5. 주사위 두 개를 던졌을 때, 눈금의 합이 6이 될 확률을 구하시오.
N <- nrow(rolldie(2))
n <- nrow(subset(rolldie(2), X1+X2 == 6))
n/N


# R Practice - 정규분포 
par(mfrow=c(1,1))

# 1. A라는 전구회사에서 생산하는 전구의 수명은 800일이고 표준편차는 40일인 정규분포를
# 따른다고 한다.  이때 전구의 수명이 750일 이하일 확률을 구하시오.
pnorm(750, 800, 40)

# 그래프
x <- seq(650, 950, by=1)
x.p <- dnorm(x, 800, 40)
plot(x, x.p, axes=F, type="l", main="N(800, 40)", ylab="")
axis(1)

lines(c(650, 950), c(0, 0))
points(750, -0.0002, pch=17, col="blue")
text(750, 0.00035, "750", col="blue")

s <- seq(650, 750, by=1)
s.z <- dnorm(s, mean=800, sd=40)
s <- c(650, s, 750)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="red", density=10, angle=305)


# 2. 어느 한 회사에 다니는 종업원들의 근무기간을 조사하였더니, 평균은 11년이고 분산이
# 16년인 정규분포를 따른다고 한다. 
mu <- 11
sigma <- 4

# 1) 20년 이상 근무한 종업원의 비율을 구하시오.
1-pnorm(20, mu, sigma)

# 그래프
x <- seq(-4, 26, by=0.001)
x.p <- dnorm(x, 11, 4)
plot(x, x.p, axes=F, type="l", main="N(11, 4)", ylab="")
axis(1)

lines(c(-4, 26), c(0, 0))
points(20, -0.002, pch=17, col="blue")
text(20, 0.0035, "20", col="blue")

s <- seq(20, 26, by=0.001)
s.z <- dnorm(s, mean=11, sd=4)
s <- c(20, s, 26)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="red", density=10, angle=305)


# 2) 근무연수가 가장 오래된 10%의 종업원은 이 회사에서 몇 년 이상 근무했다고 볼 수 있는가?
qnorm(0.9, mu, sigma)

# 3. 어느 고등학교 3학년 학생들의 수학성적은 평균이 70이고 표준편차가 8인 정규분포를 
# 따른다고 한다.  이때 점수가 80점 이상이고 90점 이하인 학생의 비율을 구하시오.
mu1 <- 70
sigma1 <- 8
pnorm(90, mu1, sigma1) - pnorm(80, mu1, sigma1)

# 그래프
x <- seq(40, 100, by=0.001)
x.p <- dnorm(x, 70, 8)
plot(x, x.p, axes=F, type="l", main="N(70, 8)", ylab="")
axis(1)

lines(c(40, 100), c(0, 0))
points(80, -0.001, pch=17, col="blue")
text(80, 0.0015, "80", col="blue")
points(90, -0.001, pch=17, col="blue")
text(90, 0.0015, "90", col="blue")

s <- seq(80, 90, by=0.001)
s.z <- dnorm(s, mean=70, sd=8)
s <- c(80, s, 90)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="red", density=10, angle=305)

# 4. 확률변수 X가 평균이 1.5, 표준편차가 2인 정규분포를 따를 때, 실수 전체의 집합에서
# 정의된 함수 H(t)는 H(t) = P(t ≤ X ≤ t+1) 이다. H(0) + H(2)의 값을 구하시오.
mu2 <- 1.5
sigma2 <- 2
H0 <- pnorm(1, mu2, sigma2) - pnorm(0, mu2, sigma2)       # H(0) = P(0 <= X <= 1)
H2 <- pnorm(3, mu2, sigma2) - pnorm(2, mu2, sigma2)       # H(2) = p(2 <= X <= 3)
H0 + H2
H0*2

# 그래프
x <- seq(-7, 10, by=0.001)
x.p <- dnorm(x, 1.5, 2)
plot(x, x.p, axes=F, type="l", main="N(1.5, 2)", ylab="")
axis(1)

lines(c(-7, 10), c(0, 0))
points(0, -0.005, pch=17, col="blue")
text(0, 0.0055, "0", col="blue")
points(1, -0.005, pch=17, col="blue")
text(1, 0.0055, "1", col="blue")
points(2, -0.005, pch=17, col="blue")
text(2, 0.0055, "2", col="blue")
points(3, -0.005, pch=17, col="blue")
text(3, 0.0055, "3", col="blue")

s <- seq(2, 3, by=0.001)
s.z <- dnorm(s, mean=1.5, sd=2)
s <- c(2, s, 3)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="red", density=10, angle=305)

s <- seq(0, 1, by=0.001)
s.z <- dnorm(s, mean=1.5, sd=2)
s <- c(0, s, 1)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col="red", density=10, angle=305)

