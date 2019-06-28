# 다중회귀모형 적합 함수 프로그램 만들기 

# 4. MASS 패키지를 설치하고, 이 패키지 안에 있는 Boston 데이터셋을 이용하여
# Boston 인근의 집값을 결정하는 다중회귀 모델을 만드시오.
library(leaps)
library(MASS)
library(stringr)

# 다중회귀모형 적합 함수
# auto_lm( data, "dependent var" )
auto_lm <- function(data, dvar) {
  
  coln <- length(colnames(data) == dvar)
  y <- data[, coln]
  
  subsets <- regsubsets(y ~ ., data = data[-coln],
                        method='seqrep', nbest=4)
  subsets <- regsubsets(y ~ ., data = data[-coln],
                        method='exhaustive', nbest=4)
  
  case <- summary(subsets)$outmat
  rown <- rownames(case)
  num1 <- str_sub(rown, 1, 1)
  num2 <- str_sub(rown, 6, 6)
  choice <- case[rown[num1 == max(num1) & num2 == 1], ] == "*"
  df_c <- as.data.frame(choice)
  df_c <- subset(df_c, df_c$choice == T)
  row_df_c <- rownames(df_c)
  form <- paste(dvar, "~")
  for (i in 1:length(row_df_c)) {
    ifelse(i < length(row_df_c), form <- paste(form, row_df_c[i], "+"), form <- paste(form, row_df_c[i]))
  }
  
  result <- lm(formula = form, data = data)
  return(result)

}

Boston
head(Boston)
model_B <- autolm(Boston, "medv")
model_B
summary(model_B)
plot(model_B)


