# Chapter 08. Practice

# One way ANOVA(일원 분산분석)
# 1. 다음은 3개 호수의 산소량의 차이가 있는지 없는지 알아보기 위하여 각 호수에서
# 10곳을 선택하여 수심 1m의 물로부터 산소량(ppm)을 측정한 자료이다.
# 3개 호수의 산소량이 같다고 할 수 있는가?
library(reshape2)
o.w1 <- data.frame(v1 = c(5, 7, 6, 8, 6, 7, 8, 8, 6, 10),
                   v2 = c(6, 8, 9, 11, 13, 12, 10, 8, 9, 10),
                   v3 = c(14, 25, 26, 18, 19, 22, 21, 16, 20, 30))
r_o.w1 <- melt(o.w1, variable.name="호수", value.name="산소량")

result1 <- oneway.test(산소량~호수, data=r_o.w1, var.equal=T)
names(result1)
result1$statistic

print(paste("One way ANOVA test 결과 검정통계량 F =", format(result1$statistic, digits=4), 
            ", p-value =", format(result1$p.value, digits=4), "이므로 유의수준 0.05보다 작아",
            "3개 호수의 산소량에 차이가 있다고 볼 수 있다."))

# 2. 다음은 3개 채소에 대한 도매시장 7곳의 가격이다.
# 3개 채소의 가격이 같다고 할 수 있는가?
o.w2 <- data.frame(v1 = c(15.5, 14.3, 16.3, 13.5, 15.7, 16.4, 14.7),
                   v2 = c(14.7, 16.3, 15.5, 15.2, 16.3, 13.5, 15.4),
                   v3 = c(15.5, 13.2, 16.5, 15.7, 15.3, 15.2, 14.8))  
r_o.w2 <- melt(o.w2, variable.name="채소", value.name="가격")

result2 <- oneway.test(가격~채소, data=r_o.w2, var.equal=T)
result2

print(paste("One way ANOVA test 결과 검정통계량 F =", format(result2$statistic, digits=4), 
            ", p-value =", format(result2$p.value, digits=4), "이므로 유의수준 0.05보다 크므로",
            "3개 채소의 가격이 같다고 할 수 있다."))

# 적합도 / 독립성 검정
# 1. 어느 공정의 부적합품률은 15% 이다. 시료 80개를 추출하여 검사한 결과 불량이 16개이다.
# 유의수준 5%로 적합도 검정을 하시오.
# 적합도 검정
pdata <- c(15, 85)
sdata <- c(16, 64)

result3 <- chisq.test(sdata, p=pdata/100)
result3
print(paste("chi-squre 적합도 test 결과 검정통계량 X-squared =", format(result3$statistic, digits=4), 
            ", p-value =", format(result3$p.value, digits=4), "이므로 유의수준 0.05보다 크므로",
            "시료 80개의 부적합품률은 15%를 만족한다고 할 수 있다."))

# 2. 다음은 음주량과 흡연량 데이터이다.
# 이 표로부터 음주량과 흡연량 사이에 연관이 있는지 확인하시오.
# 독립성 검정
df <- data.frame("1갑이상" = c(23, 31, 13),
                    "1갑이하" = c(21, 48, 23),
                    "안피움" = c(63, 159, 119))
colnames(df) <- c("1갑이상", "1갑이하", "안피움")
rownames(df) <- c("반병이상", "반병이하", "못마심")
df

result4 <- chisq.test(df)

print(paste("chi-squre 독립성 test 결과 검정통계량 X-squared =", format(result4$statistic, digits=4), 
            ", p-value =", format(result4$p.value, digits=4), "이므로 유의수준 0.05보다 작으므로",
            "음주량과 흡연량 사이에 연관이 있다고 말할 수 있다."))
