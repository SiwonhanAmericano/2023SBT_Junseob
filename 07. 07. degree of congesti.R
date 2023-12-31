library(dplyr)
library(ggplot2)

str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)

#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))

#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))

#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))


#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()

summary(congestion1$s0530)

#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <-
  rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

str(congestion1)

#2.수도권 지하철의 하루 평균 혼잡도
congestion1$day_mean
mean(congestion1$day_mean)

#3.지하철 호선별 하루 평균 혼잡도
congestion_line <-congestion1 %>%
  group_by(line) %>%
  summarise(m=mean(day_mean)) %>%
  arrange(desc(m)) %>%
  head(8)

ggplot(data=congestion_line,
       aes(x=reorder(line, m),y=m))+
  geom_col()+
  coord_flip()

congestion_line

#4.지하철 호선별 출근시간(07:00~09:00)대의 평균혼잡도
congestion1$day_mean_gtw <-
  rowMeans(congestion1[,c('s0800','s0830','s0900')])
congestion1$day_mean_gtw
mean(congestion1$day_mean_gtw)

#07시 계산
congestion1$day_mean_gtw_7 <-
  rowMeans(congestion1[c('s0700')])
mean(congestion1$day_mean_gtw_7)
summary(congestion1$day_mean_gtw_7)

#07시 30분 계산
congestion1$day_mean_gtw_7.5 <-
  rowMeans(congestion1[c('s0730')])
mean(congestion1$day_mean_gtw_7.5)
summary(congestion1$day_mean_gtw_7.5)

#08시 계산
congestion1$day_mean_gtw_8 <-
  rowMeans(congestion1[c('s0800')])
mean(congestion1$day_mean_gtw_8)
summary(congestion1$day_mean_gtw_8)

#08시 30분 계산
congestion1$day_mean_gtw_8.5 <-
  rowMeans(congestion1[c('s0830')])
mean(congestion1$day_mean_gtw_8.5)
summary(congestion1$day_mean_gtw_8.5)

#09시 계산
congestion1$day_mean_gtw_9 <-
  rowMeans(congestion1[c('s0900')])
mean(congestion1$day_mean_gtw_9)
summary(congestion1$day_mean_gtw_9)

congestion_line_gtw <-congestion1 %>%
  group_by(line) %>%
  summarise(mg=mean(day_mean_gtw)) %>%
  arrange(desc(mg)) %>%
  head(4)

ggplot(data=congestion_line_gtw,
       aes(x=reorder(line, mg),y=mg))+
  geom_col()+
  ggtitle("지하철 호선별 출근시간대의 평균 혼잡도")+
  xlab("지하철 호선")+
  ylab("평균 혼잡도")+
  coord_flip()

#5.출발시간 8시의 지하철 혼잡도 범주화/범주별 빈도분석

congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution","bad"))))%>%
  group_by(s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>% 
  select(s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%  arrange(desc(pct))%>%  head(5)

#6.지하철 호선별 퇴근시간(18:00~20:00)대의 혼잡도도
#18:00~19:00로 수정해 계산함
congestion1$day_mean_gow <-
  rowMeans(congestion1[,c('s1800','s1830','s1900')])
congestion1$day_mean_gow
mean(congestion1$day_mean_gow)

congestion_line_gow <-congestion1 %>%
  group_by(line) %>%
  summarise(mo=mean(day_mean_gow)) %>%
  arrange(desc(mo)) %>%
  head(8)

ggplot(data=congestion_line_gow,
       aes(x=reorder(line, mo),y=mo))+
  geom_col()+
  ggtitle("지하철 호선별 퇴근시간대의 평균 혼잡도")+
  xlab("지하철 호선")+
  ylab("평균 혼잡도")+
  coord_flip()

#18시 계산
congestion1$day_mean_gow_18 <-
  rowMeans(congestion1[c('s1800')])
mean(congestion1$day_mean_gow_18)
summary(congestion1$day_mean_gow_18)

#18시 30분 계산
congestion1$day_mean_gow_18.5 <-
  rowMeans(congestion1[c('s1830')])
mean(congestion1$day_mean_gow_18.5)
summary(congestion1$day_mean_gow_18.5)

#19시 계산
congestion1$day_mean_gow_19 <-
  rowMeans(congestion1[c('s1900')])
mean(congestion1$day_mean_gow_19)
summary(congestion1$day_mean_gow_19)

#19시 30분 계산
congestion1$day_mean_gow_19.5 <-
  rowMeans(congestion1[c('s1930')])
mean(congestion1$day_mean_gow_19.5)
summary(congestion1$day_mean_gow_19.5)

#20시 계산
congestion1$day_mean_gow_20 <-
  rowMeans(congestion1[c('s2000')])
mean(congestion1$day_mean_gow_20)
summary(congestion1$day_mean_gow_20)



#7.출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석

congestion1 %>%
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution","bad"))))%>%
  group_by(s18_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s18_grade=="caution")%>%
  select(s18_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

congestion1 %>%
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  group_by(line, s18_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s18_grade=="caution")%>%
  select(line, s18_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)