#Library
library(dplyr)
library(ggplot2)
library(foreign)

#SAV 입력, 데이터 프레임 생성
koweps23 <- read.spss("Koweps_h17_2022_beta1.sav")
class(mkoweps23) #객체 타입 확인
koweps23 <- as.data.frame(koweps23)

#CSV 입력
foodshop <- read.csv("ggd_food_2023.csv", na="", stringsAsFactors = F)

#변수 선택, 변수 이름 변경
foodshop <- foodshop %>%
  rename(open_date=인허가일자,
         status=상세영업상태명,
         close_date=폐업일자,
         name=사업장명, type=업태구분명,
         address=소재지전체주소) %>%
  select("name","type","status","open_date","close_date", "address")

#이상치, 결측치 제거

summary(welfare23)

table(is.na(welfare23$p_salary))
welfare23$p_salarys<-ifelse(welfare23$p_salary==0, NA, welfare23$p_salary)
table(is.na(welfare23$p_salary))

table(is.na(welfare23$t_salary))
welfare23$t_salary<-ifelse(welfare23$t_salary==64800, NA, welfare23$t_salary)
table(is.na(welfare23$t_salary))

#성별 변수 값 변경
table(welfare23$sex)
welfare23$sex<-ifelse(welfare23$sex==1, "male","female")
table(welfare23$sex)

#학력 변수 값 변경
welfare23$edu_grade<-ifelse(welfare23$edu%in%c(1,2,3,4),"중학이하",                        ifelse(welfare23$edu==5, "고교",                               ifelse(welfare23$edu==6,"전문대","대학 이상")))
table(welfare23$edu_grade)


#분석을 위해 변수 유형을 변경
mental$suicide <-as.integer(mental$suicide)

#변수 추가(birth 활용)
welfare23$age<-2023-welfare23$birth+1
range(welfare23$age)

#####################분석#################################

#상용직과 일용직 평균 총임금 분석
mean(welfare23$p_salary, na.rm = TRUE)
mean(welfare23$t_salary, na.rm = TRUE)

#상용직 근로자의 평균 총급여가 성별에 따라 차이가 있는지 t검정
#유의수준 확인 후 차이 확인
t.test(data = welfare23, p_salary~sex)

#성별 최대 총급여 상용직 근로자 찾기
welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(sex)%>%
  filter(p_salary==max(p_salary))%>%
  select(sex,edu,edu_grade,reg, p_salary)

# 연령별 평균 급여
age_salary1 <- welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(age)%>%
  summarise(m=mean(p_salary))

age_salary1%>%
  arrange(desc(m))%>%
  head(3)

#연령별 평균 총급여 그래프 작성
ggplot(data = age_salary1,aes(x=age, y=m))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

#연령별 남녀 평균 총급여 그래프 작성
age_salary2 <- welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(age, sex)%>%
  summarise(m=mean(p_salary))

#오름차순 정렬
age_salary2%>%
  arrange(desc(m))

ggplot(data = age_salary2,aes(x=age, y=m, col=sex))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

# 교육 수준별 정리
edu_salary1<-welfare23%>%
  filter (!is.na (p_salary) )%>%
  group_by (edu_grade)%>%
  summarise (m=mean(p_salary))

edu_salary1%>%
  arrange (desc(m))

ggplot(data=edu_salary1, aes(x=reorder (edu_grade, m), y=m))+ 
  geom_col ()

# 상용직 근로자의 교육수준과 성별 총급여 분석 (두 개 이상 변수)
edu_salary2<-welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by (edu_grade, sex)%>%
  summarise (m=mean(p_salary))

edu_salary2%>%
  arrange (desc(m))

ggplot(data=edu_salary2, aes(x=edu_grade,y=m,fill=sex))+
  geom_col(position="dodge")+
  scale_x_discrete(limits=c("중학이하","고교","전문대","대학 이상"))

#.성별빈도분석
mental%>%
  group_by(sex)%>%
  summarise(n=n())%>% #sex변수의 범주별 빈도 계산
  mutate(total=sum(n), #sex변수의 빈도 총계
         pct=round(n/total*100,1)) #sex변수의 범주별 비율

#.지역별 삶의 만족도 분석과 그래프 작성
area_satisfaction <-mental%>%
  group_by(area) %>%
  summarise(m=mean(satisfaction)) %>%
  arrange(desc(m))

ggplot(data=area_satisfaction, aes(x=reorder(area,m),y=m))+geom_col()+ggtitle("지역별 만족도")+xlab("지역")+ylab("만족도")+coord_flip()

#.연령대별 삶의 만족도 차이
mental%>%
  group_by(age)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))

######################## P값 다루는 거#####################

# 테이블 확인
table(mental$sex, mental$age)
# 백분율로 확인
round(prop.table(table(mental$sex, mental$age),1)*100,1)
# 교차분석
chisq.test(mental$sex, mental$age)
#.평균분석
mental%>%
  summarise(m1=mean(suicide),m2=mean(satisfaction),
            m3=mean(loneliness),m4=mean(family_belief),
            m5=mean(wealth),m6=mean(health))
#.회귀분석:삶의 만족도와 외로움이 자살충동에 미치는 영향
install.packages("car")
library(car)
RA <-lm(data=mental, suicide~satisfaction+loneliness)
summary(RA)
#다중공선성 체크
vif(RA)
#.상관분석:삶의 만족도와 외로움의 상관관계
cor.test(mental$satisfaction,mental$loneliness)
#.회귀분석:가족신뢰도, 경제안정도, 건강상태가 외로움에 미치는 영향
RA <-lm(data=mental, loneliness~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)
vif(RA)
#.독립표본t검정:성별 삶의 만족도 차이
t.test(data=mental, satisfaction~sex)

#######################

#1. 가장 오래 영업 중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% 
  
  #결측치제거,영업데이터 추출
  filter(open_date==min(open_date)) %>% 
  
  #개업일이 가장 빠른데이터 추출 
  select(name, type, open_date, address)


#2. 주요 업종별로 가장 오래 영업중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status
         =="영업") %>% 
  #결측치제거, 영업데이터 추출
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type) %>%
  #업종별분류
  filter(open_date==min(open_date)) %>% 
  #개업일이 가장 빠른 데이터 추출 
  select(name, type,open_date, address)


#3. 업종별 개업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na
         (type)&!is.na(district)) %>%
  #결측치제외
  group_by(type) %>%
  summarise(n=n()) %>%
  #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>%
  #범주별비율계산
  arrange(desc(n)) %>%
  head(10)


#4. 영업 중인 음식점의 업종별 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na
         (type)&!is.na(district)) %>%
  #결측치제외
  filter(status=="영업") %>%
  #영업만 추출
  group_by(type) %>%
  summarise(n=n()) %>%
  #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>%
  #범주별비율계산
  arrange(desc(n)) %>% head(5)


#5. 전체 음식점의 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na
         (type)&!is.na(district)) %>%
  #결측치제외
  group_by(status) %>%
  summarise(n=n()) %>%
  #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1))
#범주별비율계산


#6. 주요 업종별 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na
         (type)&!is.na(district)) %>%
  #결측치제외
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type,status) %>%
  #교차 분류
  summarise(n=n()) %>%
  #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  #범주별비율계산
  filter(status=="영업") %>%
  #영업만 추출 
  arrange(desc(n))


#7. 개업이 많았던 연도
foodshop %>%
  filter(!is.na(open_date)&!is.na
         (district))%>% #결측치제외
  group_by(open_year) %>%
  summarise(n=n()) %>%
  #범주빈도계산
  arrange(desc(n)) %>% head(5)


#8. 폐업이 많았던 연도
foodshop %>%
  filter(!is.na(close_date)&!is.na
         (district))%>% #결측치제외
  group_by(close_year) %>%
  summarise(n=n()) %>%
  #범주빈도계산
  arrange(desc(n)) %>% head(5)


#9. 연도별 개업 음식점수 그래프
#연도별 개업 음식점수
open_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na
         (district)) %>% #결측치제외
  group_by(open_year) %>%
  summarise(open_n=n())

#open_trend 구조
str(open_trend)

#연도별 개업 음식점 막대그래프
ggplot(data=open_trend,aes(x=open_year,y=open_n))+ geom_col()+ xlab("연도") +ylab("개업수")



#10. 연도별 폐업 음식점수 그래프
#연도별 폐업 음식점수
close_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na
         (district)) %>% #결측치제외
  group_by(close_year) %>%
  summarise(close_n=n())

#open_trend 구조
str(close_trend)

#연도별 개업 음식점 막대그래프
ggplot(data=close_trend,aes(x=close_year,y=close_n))+ geom_col()+ xlab("연도") +ylab("폐업수")



#11. 개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)
#연도이름변경
close_trend1<-rename(close_trend,year=close_year)
#연도이름변경
open_close_trend<-left_join(open_trend1,close_trend1,by="year")
#통합
ggplot()+
  geom_line(data=open_close_trend,aes(year,open_n))+#개업그래프
  geom_line(data=open_close_trend,aes(year,close_n,color="red"))+
  #폐업그래프 
  xlab("연도") + ylab("개수")