emp <- read_excel("data.xlsx")
emp
#group_by():변수 그룹화
#summarise() : 그룹별 통계치 산출(평균, 합계, 개수) 산출
#부서별 평균 급여
emp %>% group_by(department) %>%summarise(mean_sal = mean(salary))
emp%>%group_by(department)%>%summarise(mean_work = mean(worktime))
emp%>%group_by(position)%>%summarise(mean_work = mean(worktime))
emp%>%summarise(mean_wt = mean(worktime))
emp %>% group_by(department)%>% summarise(sum_wt = sum(worktime))
emp%>%group_by(department)%>%summarise(count_emp = n())
emp%>%group_by(department)%>%summarise(mean_sal = mean(salary), mean_wt = mean(worktime))
View(mtcars)
mtcars%>%group_by(am)%>%summarise(ave_mpg = mean(mpg), ave_disp = mean(disp))
mtcars%>%group_by(cyl)%>%summarise(count_cyl = n())
#co2
View(CO2)
str(CO2)
CO2%>%filter(Plant == 'Qn1') %>%select(Plant, conc, uptake)
CO2%>%group_by(Plant)%>%summarise(mean_uptake = mean(uptake))
df1 <- data.frame(name = c("김준호", "이민지"), birth = c(1984, 1989))
df1
df2<-data.frame(name = c("성수민", "오승준", "차현우"), birth =c(1986, 1990, 1988))
df2
df_bindRow <- bind_rows(df1, df2)
df_bindRow
df3 <- data_frame(name = c("성수민", "차현우", "이지훈"), loc = c("서울", "인천", "대전"))
df3
df_inner <- inner_join(df2, df3, by = "name")
df_inner
df_left <- left_join(df2, df3, by = "name")
df_left
df_full<-full_join(df2, df3, by = "name")
df_full
