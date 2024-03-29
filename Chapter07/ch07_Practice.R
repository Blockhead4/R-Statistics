# Chapter 07. Practice

# 2-Sample T test
# 1. mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 
# 통계적으로 유의한지 t-test를 통해 확인해 보시오.
head(mtcars)
str(mtcars)
auto <- subset(mtcars$mpg, mtcars$am==1); length(auto); mean(auto)
manu <- subset(mtcars$mpg, mtcars$am==0); length(manu); mean(manu)
sd(auto)
sd(manu)

t.test(mtcars$mpg ~ mtcars$am, mu=0, var.equal=T)

print(paste("mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 있는지",
            "알아보기 위해 표본 추출을 통해 오토 13개, 수동 19개를 측정한 결과, 오토의 mpg는"
            , format(mean(auto), digits=4), "+-", format(sd(auto), digits=4), "수동의 mpg는", 
            format(mean(manu), digits=4), "+-", format(sd(manu), digits=4), "으로 나타났습니다.",
            "이를 유의수준 0.05에서 검정하면 검정통계량과 유의확률이 -4.106(p-value=0.0003)으로 나타나",
            "기어 종류(am: 오토/수동)에 따른 mpg의 차이가 있다고 판단됩니다."))

# 2. MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이
# USA vs. non-USA 2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를
# 검정해보시오.
library(MASS)
head(Cars93)
str(Cars93)
usa <- subset(Cars93$Price, Cars93$Origin == "USA"); mean(usa); sd(usa); length(usa)
nusa <- subset(Cars93$Price, Cars93$Origin == "non-USA"); mean(nusa); sd(nusa); length(nusa)

t.test(Cars93$Price ~ Cars93$Origin, mu=0, var.equal=T)

print(paste("Cars93 데이터셋에서 생산국(Origin)이 USA vs. non-USA 2개의 group에 대해서",
            "차 가격(Price)의 평균의 차이가 있는 지를",
            "알아보기 위해 표본 추출을 통해 USA 48개, non-USA 45개를 측정한 결과, USA의 Price는",
            format(mean(usa), digits=4), "+-", format(sd(usa), digits=4), "non-USA의 Price는",
            format(mean(nusa), digits=4), "+-", format(sd(nusa), digits=4), "으로 나타났습니다.",
            "이를 유의수준 0.05에서 검정하면 검정통계량과 유의확률이 -0.9656(p-value=0.3368)으로 나타나",
            "생산국(Origin: USA / non-USA)에 따른 차 가격(Price)의 차이가 없다고 판단됩니다."))

# 3. mpg 데이터셋에서 다음을 검정해 보시오.
# 1) subcompact 자동차와 midsize 자동차의 고속도로 연비
library(ggplot2)
head(mpg)
str(mpg)

submpg1 <- subset(mpg, mpg$class == "subcompact" | mpg$class == "midsize")
scom <- subset(submpg1$hwy, submpg1$class == "subcompact"); mean(scom); sd(scom); length(scom)
mids <- subset(submpg1$hwy, submpg1$class == "midsize"); mean(mids); sd(mids); length(mids)

t.test(submpg1$hwy ~ submpg1$class, mu=0, var.equal=T)

print(paste("mpg 데이터셋에서 class subcompact vs. midsize 2개의 group에 대해서",
            "hwy(고속도로 연비)의 평균의 차이가 있는 지를",
            "알아보기 위해 표본 추출을 통해 subcompact 35개, midsize 41개를 측정한 결과, subcompact의 hwy는",
            format(mean(scom), digits=4), "+-", format(sd(scom), digits=4), "midsize의 hwy는", 
            format(mean(mids), digits=4), "+-", format(sd(mids), digits=4), "으로 나타났습니다.",
            "이를 유의수준 0.05에서 검정하면 검정통계량과 유의확률이 -0.9311(p-value=0.3548)으로 나타나",
            "class(subcompact vs midsize)에 따른 hwy(고속도로 연비)의 차이가 없다고 판단됩니다."))

# 2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
submpg2 <- subset(mpg, mpg$fl == "r" | mpg$fl == "p")
r <- subset(submpg2$cty, submpg2$fl == "r"); mean(r); sd(r); length(r)
p <- subset(submpg2$cty, submpg2$fl == "p"); mean(p); sd(p); length(p)

t.test(submpg2$cty ~ submpg2$fl, mu=0, var.equal=T)

print(paste("mpg 데이터셋에서 fl(일반 휘발유(r) vs. 고급 휘발유(p)) 2개의 group에 대해서",
            "cty(도시 연비)의 평균의 차이가 있는 지를",
            "알아보기 위해 표본 추출을 통해 일반 휘발유(r) 168개, 고급 휘발유(p) 52개를 측정한 결과, 일반 휘발유(r)의 cty는",
            format(mean(r), digits=4), "+-", format(sd(r), digits=4), "고급 휘발유(p)의 cty는", 
            format(mean(p), digits=4), "+-", format(sd(p), digits=4), "으로 나타났습니다.",
            "이를 유의수준 0.05에서 검정하면 검정통계량과 유의확률이 1.0662(p-value=0.2875)으로 나타나",
            "fl(일반 휘발유(r) vs. 고급 휘발유(p))에 따른 cty(도시 연비)의 차이가 없다고 판단됩니다."))

# 3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
submpg3 <- subset(mpg, mpg$drv == "f" | mpg$drv == "r")
submpg4 <- subset(submpg3, submpg3$class == "subcompact")
submpg4
f <- subset(submpg4$cty, submpg4$drv == "f"); mean(f); sd(f); length(f)
r <- subset(submpg4$cty, submpg4$drv == "r"); mean(r); sd(r); length(r)

t.test(submpg4$cty ~ submpg4$drv, mu=0, var.equal=T)

print(paste("mpg 데이터셋에서 drv(전륜구동(f) vs 후륜구동(r)) 2개의 group에 대해서",
            "cty(도시 연비)의 평균의 차이가 있는 지를",
            "알아보기 위해 표본 추출을 통해 전륜구동(f) 22개, 후륜구동(r) 9개를 측정한 결과, 전륜구동(f)의 cty는",
            format(mean(f), digits=4), "+-", format(sd(f), digits=4), "후륜구동(r)의 cty는", 
            format(mean(r), digits=4), "+-", format(sd(r), digits=4), "으로 나타났습니다.",
            "이를 유의수준 0.05에서 검정하면 검정통계량과 유의확률이 4.1727(p-value=0.0002)으로 나타나",
            "fl(일반 휘발유(r) vs. 고급 휘발유(p))에 따른 cty(도시 연비)의 차이가 있다고 판단됩니다."))


# Paired Sample T test

# 1. 새로운 당뇨병 치료제를 개발한 제약사에서는 치료에 지대한 영향을 주는 외부요인을 
# 통제하기 위해 10명의 당뇨병 환자를 선별하여 1달 동안 '위약(placebo)'을 투여한 기간의 
# 혈당 수치(Xi)와 '신약(new medicine)'을 투여한 1달 기간 동안의 혈당 수치(Yi)를 
# 측정하여 짝을 이루어 혈당 차이를 유의수준 5%에서 비교하시오.
placebo <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
new_medicine <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
difference <- placebo - new_medicine
difference; mean(difference); sd(difference); length(difference)

t.test(placebo, new_medicine, paired=T)

print(paste("위약(placebo)과 신약(new medicine)을 투여한 1달 기간 동안의 혈당수치의 차이가 있는지를",
            "알아보기 위해 표본 추출을 통해 10명을 대상으로 측정한 결과 평균",
            format(mean(difference), digits=4), "증가하였으며 표준편차는", format(sd(difference), digits=4),
            "로 나타났습니다. 또한 유의수준 0.05에서 검정통계량과 유의확률이 3.5507(p-value=0.006)으로 나타나",
            "위약과 신약의 혈당수치 효과는 차이가 있다고 판단됩니다."))

# 2. 두 종류의 신발 밑창의 원재료가 닳는 정도가 차이가 있는지를 검정하기 위해서 10명의 
# 소년에게 한쪽은 A라는 원재료로 만든 신발을 신기고, 다른 한쪽은 B라는 원재료로 만든 
# 신발을 신긴 후에, 일정 기간이 지난후에 신발을 수거하여 10명의 각 소년의 왼쪽 신발 
# 밑창의 닳은 정도와 오른쪽 신발 밑창의 닳은 정도의 차이를 비교하여 두 종류 원재료의
# 재질이 다른지를 검정하시오.
materialA <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
materialB <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
diff <- materialA - materialB; mean(diff); sd(diff); length(diff)

t.test(materialA, materialB, paired=T)

print(paste("A 원료(materialA)와 B 원료(materialB)의 밑창이 닳는 정도의 차이를",
            "알아보기 위해 표본 추출을 통해 10명을 대상으로 측정한 결과 평균",
            format(mean(diff), digits=4), "증가하였으며 표준편차는", format(sd(diff), digits=4),
            "로 나타났습니다. 또한 유의수준 0.05에서 검정통계량과 유의확률이 -3.3489(p-value=0.008)으로 나타나",
            "A 원료와 B 원료의 밑창이 닳는 정도는 차이가 있다고 판단됩니다."))

