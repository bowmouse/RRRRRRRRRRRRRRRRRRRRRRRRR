library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 1. 파일 불러오기 (따옴표 추가)
jeju_2019 <- read_excel("Jeju-visitors-2019.xls")
str(jeju_2019)

# 2. 열 이름 변경
jeju_2019 <- rename(jeju_2019, year = 년도, month = 월, city = 군구, loc = 관광지, kor = 내국인, forn = 외국인)

# 3. 외국인 관광객 상위 10개 관광지 (lot -> tot 변수명 통일 및 !is.na 수정)
top_loc <- jeju_2019 %>% 
  group_by(loc) %>% 
  summarise(tot = sum(forn, na.rm = TRUE))

top_10 <- top_loc %>% 
  filter(!is.na(tot)) %>% 
  arrange(desc(tot)) %>% 
  head(10)

top_10

# 그래프 1 (함수명 slcale -> scale 수정)
ggplot(top_10, aes(x = reorder(loc, tot), y = tot, fill = loc)) + 
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 500000, 50000))

# 4. 월별 내국인 관광객 추이
kor_mon <- jeju_2019 %>% 
  filter(!is.na(kor)) %>% 
  group_by(month) %>% 
  summarise(mon_tot = sum(kor, na.rm = TRUE))

head(kor_mon)

# 그래프 2
ggplot(kor_mon, aes(x = month, y = mon_tot)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  scale_y_continuous(labels = scales::comma)

# 5. 국립제주박물관 데이터 필터링 (문자열 따옴표 추가)
muse <- jeju_2019 %>% 
  filter(loc == '국립제주박물관') %>% 
  select(month, loc, kor)

muse

# 그래프 3
ggplot(muse, aes(x = month, y = kor)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  scale_y_continuous(labels = scales::comma)

jeju_2019 <- read_excel("Jeju-visitors-2019.xls")
jeju_2020 <- read_excel("Jeju-visitors-2020.xls")

jeju <- bind_rows(jeju_2019, jeju_2020)
jeju <- rename(jeju, 
               year = 년도, 
               month = 월, 
               city = 군구, 
               loc = 관광지, 
               kor = 내국인, 
               forn = 외국인)

#year변수를 문자형으로 변환
jeju$year <- as.character(jeju$year)

visit_tot <- jeju %>% filter(!is.na(kor))%>%
  group_by(year, month)%>%
summarise(mon_tot = sum(kor))
View(jeju)

#2019년과 2020년 월별 내국인 관광객 수의 선그래프
ggplot(visit_tot, aes(x = month, y = mon_tot, group = year, color = year)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  scale_y_continuous(labels = scales::comma)

#2019와 2020의 내국인 관광객 증감율 파악
visit_19 <- jeju %>%
  filter(!is.na(kor) & year == '2019') %>%
  group_by(month) %>%
  summarise(mon_19 = sum(kor))
View(visit_19)

visit_20 <- jeju %>%
  filter(!is.na(kor) & year == '2020') %>%
  group_by(month) %>%
  summarise(mon_20 = sum(kor))
View(visit_20)

visit19_20 <- left_join(visit_19, visit_20, by = "month")
View(visit19_20)

#월별 방분객 차이 데이터
vit19_20 <- left_join(visit_19, visit_20, by = "month")
View(vit19_20)
#월별 방문객 차이 퍼센트
vit_dif <- vit19_20 %>%
  mutate(dif = mon_19 - mon_20, perc = round(dif/mon_19 * 100, 2))
View(vit_dif)
ggplot(vit_dif, aes(x = month, y = perc, fill = month)) + geom_col()+scale_x_continuous(breaks = seq(1, 12, 1)) + 
  geom_hline(yintercept = c(30, 50, 70), color = "red")
#2019년 대비 2020년 4월에는 관광객이 70% 이상 감소
