# 1. 그래프

library(ggplot2)
head(cars)
par(mfrow=c(2,1))
plot(cars$speed, cars$dist,
     main="속도와 제동거리", 
     xlab="속도(mph)", ylab="제동거리(ft)", pch=1, col="red")

# jitter : 산점도를 살짝 흔들어 겹쳐있는 데이터의 가독성을 높여줌
plot(jitter(cars$speed), jitter(cars$dist),
     main="속도와 제동거리", 
     xlab="속도(mph)", ylab="제동거리(ft)", pch=1, col="red")

ggplot(cars, aes(x=speed, y=dist, col="red")) + 
  geom_jitter() +
  theme_bw()

Nile
par(mfrow=c(1,1))
plot(Nile, type="p", main="Nile강의 연도별 유량 변화",
     xlab="연도", ylab="유량", col="red")
plot.ts(Nile)
plot(Nile)

# Time series 를  Dataframe 으로 변환
df_nile <- data.frame(year=time(Nile), meter=Nile)
head(df_nile)

ggplot(df_nile, aes(x=year, y=meter)) + 
  geom_line(col="red") +
  theme_bw() + 
  ggtitle("Nile강의 연도별 유량 변화") +
  theme(plot.title = element_text(face="bold", size=14, hjust=0.5))

ggplot(df_nile, aes(x=year, y=meter)) + 
  geom_point(col="red") +
  theme_bw() + 
  ggtitle("Nile강의 연도별 유량 변화") +
  theme(plot.title = element_text(face="bold", size=14, hjust=0.5))

# 데이터 Global Environment 로 불러오기
load("D:/Workspace-JWP/R_Data_Analysis/R-Statistics/Chapter02/data/data.rda")
tableV5 <- table(data$V5)
tableV5

# 막대 그래프
barplot(tableV5, main="출생아 (남자)별 빈도", 
        xlab="출생아수", ylab="빈도")

tableV1.V4 <- table(data$V1, data$V4)
tableV1.V4
barplot(tableV1.V4, legend.text = T, col=c("orange", "green"),
        main="학력에 따른 성별 인원수", xlab="학력", ylab="빈도")

# 히스토그램
par(mfrow=c(1,2))
hist(data$V2, main="연령별 분포", 
     xlab="연령", ylab="빈도", col=rainbow(18))
hist(data$V2, probability=T, main="연령별 분포",      # probability=T : 빈도가아닌 비중(밀도)
     xlab="연령", ylab="밀도", col=rainbow(18))   
par(mfrow=c(1,1))
hist(data$V2, breaks=c(seq(0, 90, 10)), right=F, main="연령별 분포",    # right=F : 시작점 이상 끝점 미만
     xlab="연령", ylab="빈도", col=rainbow(9))

# 원 도표
table(data$V4)
pie(table(data$V4), main="학력수준별 비중", cex=1)
