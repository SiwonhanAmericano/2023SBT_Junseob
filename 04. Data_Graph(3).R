#2 Step. (데이터 분석) X2023_STB_survey의 Gender 1개의 인자를 가지고 도수분포표를 작성하세요.

table(X2023_STB_survey $Gender)

#3 Step. (데이터 분석) X2023_STB_survey의 Gender 1개의 인자를 가지고 상대도수분포표를 작성하세요.

GEN <- table(X2023_STB_survey $Gender)
prop.table(GEN)

#4 Step. (데이터 분석) X2023_STB_survey의 Gender와 Grade 2개의 인자를 가지고 교차표를 작성하세요.

table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

#5 Step. (데이터 분석) X2023_STB_survey의 Nationality 1개의 인자를 가지고 막대그래프를 작성하세요.

barplot(table(X2023_STB_survey $Nationality))

#6 Step. (데이터 분석) X2023_STB_survey의 residential area 1개의 인자를 가지고 (가로)막대그래프를 작성하세요.

barplot(table(X2023_STB_survey $'residential area'), horiz=TRUE)

#7 Step. (데이터 분석) X2023_STB_survey의 Gender와 Grade 2개의 인자를 가지고 막대그래프를 작성하세요.

entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE)

#8 Step. (데이터 분석) X2023_STB_survey의 Grade 1개의 인자를 가지고 파이차트를 작성하세요.

pie(table(X2023_STB_survey $Grade))

#9 Step. (데이터 분석) X2023_STB_survey의 Age 인자를 가지고 히스토그램을 작성하세요.

hist(X2023_STB_survey $`Age`, main="연령 분포", col=terrain.colors(12))

#10 Step. (데이터 분석) X2023_STB_survey의 Grade별 Age를 비교하는 박스 플롯을 만들어 보세요. 그리고 Grade별 Age에 대한 기술통계분석을 실시하여 각 박스 플롯을 비교 설명하세요.


boxplot(week4_A$`Grade: 2`,week4_A$`Grade:3`,week4_A$`Grade:4`, main="학년별 연령분포 현황", col="yellow", names = c("2학년","3학년","4학년"))


#11 Step. (데이터 분석) X2023_STB_survey의 Grade를 X값으로 Age를 Y값으로 하는 산점도를 만들어 보세요

plot(x=X2023_STB_survey$`Grade`, y=X2023_STB_survey$`Age`, xlab="학년", ylab="나이", main="학년별 연령분포 현황")

