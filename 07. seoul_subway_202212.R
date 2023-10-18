install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

str(subway)

summary(subway) #세부과제1은 결측치가 없음/ 세부과제2는 결측치가 존재함


#파생변수
subway$day <-substr(subway$Date,7,8) #7,8번째 자리 추출출
class(subway$day) #character이므로 inteager로 변환환
subway$day <-as.integer(subway$day)

table(subway$Line) #9호선 2~3단계를 합칠 것임
subway$Line <-ifelse(subway$Line=="9호선2~3단계","9호선",subway$Line)
table(subway$Line)


table(subway$Station) #가오리역이 있는지? 직접 확인해보자

subway$total_passenger<-subway$on_board+subway$getting_off#
total_passenger#

str(subway)

subway%>%
  summarise(on_m=mean(on_board), off_m=mean(getting_off))

max(subway$on_board)

subway%>%
  filter(on_board==94732)%>%
  select(Date, Line, Station,on_board)


passenger10 <- subway %>%
  group_by(Station)%>%
  summarise(m=mean(total_passenger))%>%
  arrange(desc(m))%>%
  head(10)

head(passenger10, 3)

ggplot(data=passenger10, aes(x=reorder(Station, m),y=m))+
  geom_col()+
  coord_flip() #회전해서 그래프 작성


subway %>%
  group_by(Date) %>%
  summarise(total=sum(total_passenger)) %>%
  arrange(desc(total)) %>%
  head(3) # 도출된 일자의 요일을 파악해보자

subway %>%
  filter(Line=="4호선") %>% #호선을 바꾸어 왜 그 날에 많았는지 유추해보자
  filter(total_passenger==max(total_passenger)) %>%
  select(Date, Station, on_board,
         getting_off, total_passenger)


line_pct <- subway %>%
  group_by(Line) %>%
  summarise(total=sum(total_passenger)) %>%
  mutate(all=sum(total), #처음보는 함수이니 자세히 보자
         pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3) #어느 역을 가장 많이 사용했나요?



#지하철전체승객비율막대그래프그리기

line_pct10 <- line_pct %>%
  filter(Line%in%c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선","9호선","분당선" ))

ggplot(data = line_pct10, aes(x=reorder(Line,pct),y=pct))+
  geom_col()+
  coord_flip()+
  ggtitle("수도권 지하철 노선별 이용비율")+
  xlab("노선")+
  ylab("이용비율")



#일별전체승객선그래프그리기

line_graph <- subway %>%
  group_by(day) %>%
  summarise(s=sum(total_passenger))

ggplot(data = line_graph, aes(x=day, y=s, group=1))+
  geom_line()+
  ggtitle("수도권 지하철 일별 이용승객수")+
  xlab("일")+
  ylab("이용승객")

#왜 이런 결과들이 나왔을까요?


