# 9-2. 회귀분석
# 2) 아버지와 아들 키 자료로부터 회귀계수 추정
setwd("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter09")
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header=T, stringsAsFactors=F)
plot(hf$Height, hf$Father)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender == "M")
hf.son <- hf.son[c("Father", "Height")]

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father - mean.x) * (hf.son$Height - mean.y))
sxx <- sum((hf.son$Father - mean.x)^2)

b1 <- sxy / sxx
b0 <- mean.y - b1 * mean.x

# lm() 함수 이용
out <- lm(Height ~ Father, data=hf.son)
summary(out)

par(mar=c(2,2,2,2))
par(mfrow=c(2,2))
plot(out)
# 좋은 선형 모델
# 정규성 - 두번쩨 그림
# 독립성
# 선형성 - 첫번째 그림
# 등분산성 - 세번째 그림
par(mfrow=c(1,1))


# Polynomial Regression
women    # height(inch), weight(lbs)

# 신장에 따른 몸무게
plot(weight~height, data=women)
fit <- lm(weight~height, data=women)
abline(fit, col="red", lwd=2)

summary(fit)
cor.test(women$weight, women$height)

par(mfrow=c(2,2))
plot(fit)    # 정규성(2-x), 독립성, 선형성(1-x), 등분산성(3-o)
par(mfrow=c(1,1))

fit2 <- lm(weight~height + I(height^2), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit2), col="green", lwd=2)

summary(fit2)
par(mfrow=c(2,2))
plot(fit2)   # 정규성(2-o), 독립성, 선형성(1-o), 등분산성(3-o)
par(mfrow=c(1,1))

fit3 <- lm(weight~height + I(height^2) + I(height^3), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit3), col="orange", lwd=2)

summary(fit3)
par(mfrow=c(2,2))
plot(fit3)   # 정규성(2-x), 독립성, 선형성(1-o), 등분산성(3-o)
par(mfrow=c(1,1))

# AIC(Akaike Information Criterion)
AIC(fit, fit2, fit3)     # 값이 작을수록 좋은 모델


# 다중 회귀 분석
state.x77
head(state.x77)
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

fit1 <- lm(Murder ~ ., data=states)
summary(fit1)

fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

AIC(fit1, fit2)

# Backward stepwise regression, Forward stepwise regression
step(fit1, direction = "backward")

fit3 <- lm(Murder ~ 1, data=states)
step(fit3, direction = "forward",
     scope=~ Population + Illiteracy + Income + Frost)
step(fit3, direction = "forward",
     scope= list(upper=fit1, lower=fit3))

# 모든 경우에 대한 비교 : regsubsets
install.packages("leaps")
library(leaps)
subsets <- regsubsets(Murder~., data=states,
                      method='seqrep', nbest=4)
subsets <- regsubsets(Murder~., data=states,
                     method='exhaustive', nbest=4)
summary(subsets)
plot(subsets)

# Logistic Regression
data <- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
str(data)
head(data)

data$rank <- as.factor(data$rank)
str(data)

train <- data[1:200,]
test <- data[201:400,]
model <- glm(admit ~ gre + gpa + rank, data=data, family="binomial")
summary(model)

model2 <- glm(admit ~ gpa + rank, data=data, family="binomial")
summary(model2)

AIC(model, model2)
