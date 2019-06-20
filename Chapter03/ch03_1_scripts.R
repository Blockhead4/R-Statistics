# 3. 확률과 확률분포
# 3-1. 확률
install.packages("prob")
library(prob)

tosscoin(3)     # 동전을 n번 던졌을 때 나올 수 있는 경우의 수
rolldie(2)      # 주사위를 n번 던졌을 때 나올 수 있는 경우의 수
urnsamples(1:3, size=2)     # 입력한 벡터 값의 개별 원소들로 구성될 수 있는 경우의 수
urnsamples(1:3, size=2, replace=T)     # 복원 추출일 경우
urnsamples(c(rep("R", 3), rep("B", 2)), size=2)    # 빨간공 3개, 파란공 2개, 총 5개 중에서 2개를 뽑는 경우의 수
choose(5,2)     # 5 combination 2
tosscoin(2, makespace = T)     # 확률을 계산하여 컬럼에 추가

# 확률변수의 평균과 기대값
x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
EX <- sum(x * px)
EX

x2 <- x ^ 2
EX2 <- sum(x2 * px)    # 제곱의 기대값
EX2
VX <- EX2 - EX^2    
VX
