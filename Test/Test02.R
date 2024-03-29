# R Test. 02
options(digits=4)

# 1. 다음의 값을 R 내장함수를 이용하여 구하시오. (10점)
# 1) 시행횟수가 6이고 성공확률이 1/3인 이항분포에서 성공횟수가 3이 될 확률
dbinom(3, 6, 1/3)

# 2) 평균이 170이고 표준편차가 6인 정규분포에서 상위 20%되는 사람들의 키 범위
qnorm(0.8, 170, 6)

# 3) 자유도가 3인 카이제곱분포에서 누적확률이 95%일 때의 값
qchisq(0.95, df=3)

# 4) 표준정규분포에서 확률변수의 값이 1일때의 누적확률
pnorm(1)

# 2. 다음의 문항이 베루누이 시행인지 판단하시오. (10점)

# 1) 영화관에서 줄을 기다리는 시간을 측정한다.
print("정답 : X")

# 2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다.
print("정답 : O")

# 3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다.
print("정답 : X")

# 4) 동전의 앞면이 위로 향하고 있는지 체크한다.
print("정답 : O")

# 5) 사람의 눈이 녹색인지 체크한다.
print("정답 : O")

# 3. R 내장 데이터인 "iris"를 이용하여 다음을 구하시오. (10점)
# - "setosa" 중 Sepal.Length의 모평균에 대한 95% 신뢰구간

iris_s <- subset(iris, iris$Species == "setosa")
iris_s <- iris_s$Sepal.Length

s.mean <- mean(iris_s)
s.n <- length(iris_s)
s.var <- mean(iris_s^2) - s.mean^2
s.sd <- sqrt(s.var)
alpha <- 0.05

ll <- s.mean - qnorm(1-alpha)*s.sd/sqrt(s.n)
ul <- s.mean + qnorm(1-alpha)*s.sd/sqrt(s.n)

print(paste("95% 신뢰구간 : (", format(ll, digits=4), ",", format(ul, digits=4), ")"))

# 4. 한 농구 선수가 자유투를 던지면 10번 중에서 7번을 성공한다고 할 때
# 다음을 R을 이용하여 풀이하시오. (10점)
# 1) 이 선수가 자유투를 10번 던져서 9번 이상 성공할 확률을 구하시오.
1-pbinom(8, 10, 0.7)
      
# 2) 이 선수가 자유투를 10번 던질 때 5번 이상 8번 이하로 성공할 확률을 구하시오.
pbinom(8, 10, 0.7) - pbinom(4, 10, 0.7)

# 5. 다음을 R을 이용하여 검정하시오. (10점)
# - 2006년 조사에 의하면 한국인의 1인 1일 평균 알코올 섭취량이 8.1g 이다.
#   2008년 무작위로 뽑은 알코올 섭취량은 다음과 같다.
#     16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97
#   평균 알코올 섭취량이 달라졌다고 할 수 있는가?
alchol <- c(16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)
result1 <- t.test(alchol, mu=8.1, var.equal = T)

print(paste("T test 결과 검정통계량 및 유의확률이 (", format(result1$statistic, digits=4)
            , ",", format(result1$p.value, digits=4), ") 이므로 유의수준 0.05에서",
            "평균 알코올 섭취량이 달라졌다고 할 수 없다."))

# 6. 정규분포에서 from <= X <= to 의 확률을 구하는 함수 rangenorm(from, to, mean, sd)을 작성하고
# rangenorm(-1.96, 1.96, 0, 1)의 값을 구하시오. (10점)
rangenorm <- function(from, to, mean=0, sd=1) {
  result <- pnorm(to, mean, sd) - pnorm(from, mean, sd)
  return(result)
}
rangenorm(-1.96, 1.96, 0, 1)


# 7. mpg 데이터셋에서 다음을 검정해 보시오. (10점)
# 1) subcompact 자동차와 midsize 자동차의 도시연비
library(ggplot2)
sub_mpg1 <- subset(mpg, mpg$class=="subcompact" | mpg$class == "midsize")
var.test(sub_mpg1$cty ~ sub_mpg1$class)
result2 <- t.test(sub_mpg1$cty ~ sub_mpg1$class, var.equal=T)

print(paste("T test 결과 검정통계량 및 유의확률이 (", format(result2$statistic, digits=4)
            , ",", format(result2$p.value, digits=4), ") 이므로 유의수준 0.05에서",
            "subcompact 자동차와 midsize 자동차의 도시연비는 차이가 있다고 할 수 있다."))

# 2) 일반 휘발유(r)와 고급 휘발유(p)의 고속도로 연비
sub_mpg2 <- subset(mpg, mpg$fl == "r" | mpg$fl == "p")
var.test(sub_mpg2$hwy ~ sub_mpg2$fl)
result3 <- t.test(sub_mpg2$hwy ~ sub_mpg2$fl, var.equal=T)

print(paste("T test 결과 검정통계량 및 유의확률이 (", format(result3$statistic, digits=4)
            , ",", format(result3$p.value, digits=4), ") 이므로 유의수준 0.05에서",
            "일반 휘발유(r)와 고급 휘발유(p)의 고속도로 연비는 차이가 있다고 할 수 있다."))

# 8. 다음을 R을 이용하여 적합도를 검정하시오. (10점)
# - 멘델이 제안한 유전법칙에 의하면 순종의 둥글고 황색인 완두(RRYY)콩과 주름지고
#   녹색인 완두(rryy)콩을 재배하면 2대째 발현되는 완두콩의 형질은 둥글고 황색, 
#   둥글고 녹색, 주름지고 황색, 주름지고 녹색이 9 : 3 : 3 : 1 의 비율로 나타난다고
#   한다. 
#   멘델의 유전법칙이 맞는지 확인하기 위해 실험한 결과 각 형질별 개체수가 순서대로
#   322, 109, 99, 29 개가 나타났다.
#   실험 결과는 멘델이 제안한 비율 9 : 3 : 3 : 1 의 비율에 맞게 나타난 것인지
#   통계적 가설검정을 하시오.
s_data <- c(322, 109, 90, 29)
result4 <- chisq.test(s_data, p=c(9, 3, 3, 1)/16)

print(paste("Chi-Square test 결과 검정통계량 및 유의확률이 (", format(result4$statistic, digits=4)
            , ",", format(result4$p.value, digits=4), ") 이므로 유의수준 0.05에서",
            "관측치의 비율과 멘델이 제안한 비율( 9 : 3 : 3 : 1 )이 차이가 없다고 할 수 있다."))

# 9. R 내장 데이터인 "women"을 이용하여 다음을 구하시오. (10점)
# - 키(Height)와 몸무게(Weight)의 곡선회구분석을 통한 회귀식
# (단, 2차식으로 구할 것.)

shapiro.test(women$weight)
shapiro.test(women$height)
par(mfrow=c(1,2))
qqnorm(women$weight); qqline(women$weight)
qqnorm(women$height); qqline(women$height)
var.test(women$height, women$weight)

result5 <- lm(women$weight ~ women$height + I(women$height^2), data=women)
summary(result5)
par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(result5)
par(mfrow=c(1,1))

print(paste("2차 회귀모델을 적용한 결과 모든 독립변수가 매우 유의한것으로 보이고",
            "잔차그래프에서 정규성, 등분산성, 선형성을 만족하는 것으로 보여 적합한",
            "모델이라고 볼 수있다.", "\n",
            "회귀식은 [ Weight = 261.8781 - 7.3483*Height + 0.0831*Heigth^2 ] 이다."))

# 10. 과제점수 (10점)