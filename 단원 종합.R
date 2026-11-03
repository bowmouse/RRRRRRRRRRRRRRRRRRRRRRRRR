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
#ggplot2 패키지의 ggplot()
library(ggplot2)
library(dplyr)
#geom_col() : 막대그래프

View(Titanic)
str(Titanic)
df <- as.data.frame(Titanic)
str(df)
#타이타닉 선실별 생존자 합계
df_sur <- df%>%filter(Survived == "Yes")%>%group_by(Class) %>% summarise(sum = sum(Freq))
df_sur
pie(df_sur$sum, labels = df_sur$Class, col = c("blue", "green", "yellow", "red"), main = "Titanic Survivor", init.angle = 90)
#선실별 사망자 합계표, 파이 차트
df_die <- df%>%filter(Survived == "No")%>%group_by(Class) %>% summarise(sum = sum(Freq))
df_die
pie(df_die$sum, labels = df_die$Class, col = c("blue", "green", "yellow", "red"), main = "Titanic ghost", init.angle = 90)
#geom_col() : 막대그래프
#타이타닉 선실별 생존자 그래프(생존자통계 : df_sur)
ggplot(data = df_sur, aes(x = Class, y = sum, fill = Class)) + geom_col()

#geom_bar() : 빈도막대그래프
View(diamonds)
#다이아몬드 연마등급에 따른 빈도 막대그래프
ggplot(diamonds, aes(x = cut, fill = cut)) + geom_bar()
#geom_histogram() : 히스토그램
#다이아몬드 무게(carat)에 다한 구간별 히스토그램
ggplot(diamonds, aes(x = carat)) + geom_histogram(fill = "steelblue", color = "white", binwidth = 0.1) + xlim(0, 3)
#geom_line() : 선그래프
#다이아몬드 무게별 개수
dia_count <- diamonds %>% group_by(carat) %>% summarise(num = n())
dia_count
ggplot(dia_count, aes(x = carat, y = num)) + geom_line() + xlim(0, 3)
#geom_point() : 점그래프
View(iris)
#붓꽃의 꽃받침 길이에 따른 꽃받침 너비 분포
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point()
#geom_boxplot() : 상자그림그래프
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Species)) + geom_boxplot()
