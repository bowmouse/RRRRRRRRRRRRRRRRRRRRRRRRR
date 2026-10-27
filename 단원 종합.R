#chapter 6
data <- data.frame(group = c("A", "B", "NA", "C", "D", NA), prod = c(20, 10, 05, NA, 30, NA))

data
#결측지 확인
is.na(data)

#결측지 개수 확인
table(is.na(data))

#결측지를 포함한 데이터의 계산결과
sum(data$prod)

#특정변수의 결측지 개수 확인
table(is.na(data$group))
table(is.na(data$prod))

#결측지가 없는 행 출력하기
library(dplyr)
data %>% filter(!is.na(group))
data %>% filter(!is.na(group) & !is.na(prod))

#여러 변수의 결측치 모두 제거하기
na.omit(data)

#결측치 제거하고 통곗값 구하기
data %>% summarise(m = mean(prod)) #결측치가 있으면 NA
data %>% summarise(m = mean(prod, na.rm = T))

#결측치를 다른 값으로 대체
mean <- mean(data$prod, na.rm = T)
mean
data$prod <- ifelse(is.na(data$prod), mean, data$prod)
data

##데이터셋으로 복습
View(airquality)
table(is.na(airquality$Ozone))

#결측치를 구하고 평균 구하기
airquality %>% summarise(mean_Ozone = mean(Ozone, na.rm = T))

##결측치를 평균값으로 대체
#airq 복사본으로 실습
airq <- airquality
airq$Ozone <- ifelse(is.na(airq$Ozone), 42, airq$Ozone)
table(is.na(airq$Ozone))

#6.2이상 데이터 처리
data <- data.frame(gen = c(2, 1, 2, 1, 3, 2, 1), grade = c("A", "B", "B", "D", "A", "B", "B"))
data

table(data$gen)
table(data$grade)
boxplot(data$gen)
data <- c(0, 4, 5, 6, 6, 7, 7, 7, 10, 14, 15 )
boxplot(data)
data

#1. 이상치를 데외하나 데이터 통계값
boxplot(data)$stat

#2. 이상치 제거(이상치를 NA로 대체)
data <- ifelse(data<4 | data > 10, NA, data)
data

#3. NA 제거 후 평균
mean(data, na.rm = T)

##airq로 실습
#wind변수 이상치 확인, 이상치 처리(1.~3.)
boxplot(airq$Wind)
boxplot(airq$Wind)$stat #1.
airq$Wind <- ifelse(airq$Wind<1.7 | airq$Wind>16.6, NA, airq$Wind) #2.
table(is.na(airq$Wind))
airq %>%summarise(mean_Wind = mean(Wind, na.rm = T)) #3.

#7.1 low-level 그래프$ggplot2
#pie(), barplot()...
company <- c("A사", "B사", "C사")
sales <- c(30, 55, 40)
df <- data.frame(company, sales)
df
pie(df$sales, labels = df$company, col = c("red", "blue", "green"), main = "회사별 판매량", clockwise = T)
