#최종#
data<-read.csv('E:/tic2000.csv',header=TRUE)
data=data[-1]


###변수이름바꾸기

names(data)<-c(
"X1",  "X2",  "X3",  "X4",  "X5",  "X6",  "X7",  "X8",  "X9",  "X10", "X11",
"X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "X20", "X21", "X22", "X23",
"X24", "X25", "X26", "X27", "X28", "X29", "X30", "X31", "X32", "X33", "X34", "X35",
"X36", "X37", "X38", "X39", "X40", "X41", "X42", "X43", "X44", "X45", "X46", "X47",
"X48", "X49", "X50", "X51", "X52", "X53", "X54", "X55", "X56", "X57", "X58", "X59",
"X60", "X61", "X62", "X63", "X64", "X65", "X66", "X67", "X68", "X69", "X70", "X71",
"X72", "X73", "X74", "X75", "X76", "X77", "X78", "X79", "X80", "X81", "X82", "X83",
"X84", "X85", "X86")

data$X86 <- factor(data$X86)
data$X1 <- factor(data$X1)
data$X5 <- factor(data$X5)

str(datatest)
datatest<-data[,1:43]
plot(datatest)
boxplot(datatest)

aaa<-data$X86
barplot(table(aaa))

##################eda###############################
head(data)

#X86차이보여주기
aaa<-data$X86
barplot(table(aaa))

#1 고객하위분류테이블
table(data$X1)
#2 우편번호안의 가구수
table(data$X2)
#3~4
hist(data[,3])
barplot(table(data[,3]))
barplot(table(data[,4]), names=c("20대","30대","40대","50대","60대","70대"))
#5
table(data[,5])
#6~9
boxplot(data[,6:9])

#10~12
boxplot(data[,10:12],names=c("결혼","동거","그외"))

library(ggplot2)

ggplot(data[,10:12], aes(color, fill=cut))
    geom_bar(position='fill')

ggplot(data,aes(data[,10:12],count,fill=age))+geom_bar(position='fill',stat='identity')

pie(aa10)
aa10<-data[,10:12]

#13~15
boxplot(data[,13:15],names=c("싱글","자녀없음","자녀있음"))
barplot(data[,13:15], col=rainbow(5),
        beside = T, horiz = T,
        legend = row.names(data[,13:15]))
hist(table(data[,13:15]))

#16~18
boxplot(data[,16:18],names=c("높음","중간","낮음"))

###################################################
##19~24
boxplot(data[,19:24])
hist(table(data[,19:24]))
plot(table(data[,19]))
a<-sum(data[,19])
b<-sum(data[,20])
c<-sum(data[,21])
d<-sum(data[,22])
e<-sum(data[,23])
f<-sum(data[,24])
label<-c("X19","X20","X21","X22","X23","X24")
aaaa<-c(a,b,c,d,e,f)
pie(x=aaaa, labels=label, clockwise=TRUE) 

#25~29
boxplot(data[,25:29])
a2<-sum(data[,25])
b2<-sum(data[,26])
c2<-sum(data[,27])
d2<-sum(data[,28])
e2<-sum(data[,29])
label2<-c("X25","X26","X27","X28","X29")
aaaa2<-c(a2,b2,c2,d2,e2)
pie(x=aaaa2, labels=label2, clockwise=TRUE) 

#30~31
boxplot(data[,30:31], names=c())
pie(table(data[,30:31]))
barplot(table(data[,30]))
barplot(table(data[,31]))

#32~34
boxplot(data[,32:34])

#35~36
boxplot(data[,35:36], names=c("공보험","사보험"))
a3<-sum(data[,35])
b3<-sum(data[,36])
label3<-c("공보험","사보험")
aaaa3<-c(a3,b3)
pie(x=aaaa3, labels=label3, clockwise=TRUE) 

#37~41
aa37<-data[,37:41]
aa37<-prop.table(aa37)
barplot(table(data[,37]))
boxplot(data[,37:41])

#42
head(data[,42])
barplot(table(data[,42]))

#43
barplot(table(data[,43]))

#44~64
barplot(data[,44:64])

ggplot() + geom_bar(data=aa44, aes(y=mpg), stat='identity')

#65~85
a5<-(data[,65:85])

plot(a4,a5)



head(data[,44:85])


a5<-matrix(0,21,1)
for (i in 44:64){
a5[(i-43),]<-sum(data[,i])
i=i+1
}


#######################################################
###################변수50:50으로정리하기###################
#######################################################

*install.packages("ROSE")
library(ROSE)

table(data$X86)
data<- ovun.sample(X86 ~ ., data = data, method = "under",N = 586*2,seed = 2021)$data

head(data)
str(data)

#################################################################################
##############################변수정리############################################
#################################################################################

#X4_1 변수 생성-나이를실제값으로대입
data$X4_1<-data$X4
data$X4_1<-replace(data$X4_1,data$X4_1==1,25)
data$X4_1<-replace(data$X4_1,data$X4_1==2,35)
data$X4_1<-replace(data$X4_1,data$X4_1==3,45)
data$X4_1<-replace(data$X4_1,data$X4_1==4,55)
data$X4_1<-replace(data$X4_1,data$X4_1==5,65)
data$X4_1<-replace(data$X4_1,data$X4_1==6,75)
table(data$X4)
table(data$X4_1)
str(data)

#X9_1 변수 생성-종교를가질확률
#방법1
nrow(data)
data$X9_1<-matrix(0,nrow=1172)
str(data)
data$X9_1<-(data$X6+data$X7+data$X8)/((data$X6+data$X7+data$X8+data$X9))
str(data$X9_1)
summary(data$X9_1)


#X10_1 변수 생성-결혼했을확률
#방법1
data$X10_1<-matrix(0,nrow=1172)
str(data)
data$X10_1<-(data$X10)/((data$X10+data$X11+data$X12))
str(data$X10_1)
summary(data$X10_1)

#X15_1 변수 생성-아이를가지고있을확률
data$X15_1<-matrix(0,nrow=1172)
str(data)
data$X15_1<-(data$X15)/((data$X13+data$X14+data$X15))
str(data$X15_1)
summary(data$X15_1)

#X16_1 변수 생성-교육수준
data$X16_1<-matrix(0,nrow=1172)
data$X16_1<-(data$X16-data$X18)/((data$X16+data$X17+data$X18))
str(data$X16_1)
summary(data$X16_1)
str(data)

#교육수준과
edu_logit <- glm(X86 ~ X16_1 , data=data, family = "binomial")
summary(edu_logit)

#X19~X24-특정직업이차지하는비율구하기
data$X19_1<-matrix(0,nrow=1172)
data$X19_1<-(data$X19)/((data$X19+data$X20+data$X21+data$X22+data$X23+data$X24))

data$X20_1<-matrix(0,nrow=1172)
data$X20_1<-(data$X20)/((data$X19+data$X20+data$X21+data$X22+data$X23+data$X24))

data$X21_1<-matrix(0,nrow=1172)
data$X21_1<-(data$X21)/((data$X19+data$X20+data$X21+data$X22+data$X23+data$X24))

data$X22_1<-matrix(0,nrow=1172)
data$X22_1<-(data$X22)/((data$X19+data$X20+data$X21+data$X22+data$X23+data$X24))

data$X23_1<-matrix(0,nrow=1172)
data$X23_1<-(data$X23)/((data$X19+data$X20+data$X21+data$X22+data$X23+data$X24))

data$X24_1<-matrix(0,nrow=1172)
data$X24_1<-(data$X24)/((data$X19+data$X20+data$X21+data$X22+data$X23+data$X24))

str(data$X19_1)
summary(data$X19_1)

str(data$X20_1)
summary(data$X20_1)

str(data$X21_1)
summary(data$X21_1)

str(data$X22_1)
summary(data$X22_1)

str(data$X23_1)
summary(data$X23_1)

str(data$X24_1)
summary(data$X24_1)

#X31_1 변수 생성-집 소유 여부
data$X31_1<-matrix(0,nrow=1172)
data$X31_1<-(data$X31)/((data$X30+data$X31))
str(data$X31_1)
summary(data$X31_1)

#X32_1 변수 생성-가진 차의 평균 개수
data$X32_1<-matrix(0,nrow=1172)
data$X32_1<-(data$X32+2*data$X33)/((data$X32+data$X33+data$X34))
str(data$X32_1)
summary(data$X32_1)

#X35_1 변수 생성-공보험에가입할확률
data$X35_1<-matrix(0,nrow=1172)
data$X35_1<-(data$X35*1+data$X36*(-1))/((data$X35+data$X36))
str(data$X35_1)
summary(data$X35_1)

#X37_1 변수 생성-수입을 1~5 단계로 나누어 표현(연속)
data$X37_1<-matrix(0,nrow=1172)
data$X37_1<-(data$X37*1+data$X38*2+data$X39*3+data$X40*4+data$X41*5)/((data$X37+data$X38+data$X39+data$X40+data$X41))
str(data$X37_1)
summary(data$X37_1)

#X38_1 변수 생성-수입을 숫자형으로 최저를 기준으로 평균을 계산함
data$X38_1<-(data$X37*0+data$X38*30000+data$X39*45000+data$X40*75000+data$X41*123000)/(data$X37+data$X38+data$X39+data$X40+data$X41)
str(data$X38_1)
summary(data$X38_1)

#X42변수와 X43변수사이의 상관계수->X43(구매력)이 높은 숫자로 표현될수록 수입이 많은것임을 확인
cor(data$X42,data$X43)

#보험의 개수 

#가구당평균보험료구하기
data$X44_mean<-data$X44/data$X65


#V45과 V46이 서로 연관성이있는지 확인-없음
cor(data$X45,data$X46)

#############################################################################
#############################################################################

#소득분위 바꿔주기(0->10)
#새로운 소득분위 변수 X42_1 생성
cor(data$X38_1,data$X42)

table(data$X42)
data$X42_1<-data$X42
data$X42_1<-replace(data$X42_1,data$X42_1==0,10)
table(data$X42_1)

cor(data$X38_1,data$X42_1)
plot(data$X38_1,data$X42_1) #10으로 갈수록 수입이 더 높음

head(data)

#V44-V64 변수 돈으로 바꿔서 표현하기

Xmoney=matrix(0,length(data[,1]),21)

i=44
for (t in 1:21) {
for (j in 1:length(data[,1])){
if (data[j,i]==0){
Xmoney[j,t]=0
}
else if (data[j,i]==1) {
Xmoney[j,t]=1
}
else if (data[j,i]==2) {
Xmoney[j,t]=50
}
else if (data[j,i]==3) {
Xmoney[j,t]=100
}
else if (data[j,i]==4) {
Xmoney[j,t]=200
}
else if (data[j,i]==5) {
Xmoney[j,t]=500
}
else if (data[j,i]==6) {
Xmoney[j,t]=1000
}
else if (data[j,i]==7) {
Xmoney[j,t]=5000
}
else if (data[j,i]==8) {
Xmoney[j,t]=10000
}
else{
Xmoney[j,t]=20000
}
}
i=i+1
}

str(data.frame(Xmoney))
str(Xmoney)
Xmoney<-data.frame(Xmoney)
names(Xmoney)<-c("X44_money","X45_money","X46_money","X47_money","X48_money","X49_money","X50_money"
                 ,"X51_money","X52_money","X53_money","X54_money","X55_money","X56_money","X57_money"
                 ,"X58_money","X59_money","X60_money","X61_money","X62_money","X63_money","X64_money")


data<-cbind(data,Xmoney)
head(data)

#평균보험료계산하기
Xmeanmoney=matrix(0,length(data[,1]),21)
i=44
for (t in 1:21) {
for (j in 1:length(data[,1])){
if (data[j,i+21]==0){
Xmeanmoney[j,t]=0
}
else{
Xmeanmoney[j,t]=Xmoney[j,t]/data[j,i+21]
}
}
i=i+1
}

str(data.frame(Xmeanmoney))
head(Xmeanmoney)
Xmeanmoney<-data.frame(Xmeanmoney)
names(Xmeanmoney)<-c("X44_mean.money","X45_mean.money","X46_mean.money","X47_mean.money","X48_mean.money","X49_mean.money","X50_mean.money"
                 ,"X51_mean.money","X52_mean.money","X53_mean.money","X54_mean.money","X55_mean.money","X56_mean.money","X57_mean.money"
                 ,"X58_mean.money","X59_mean.money","X60_mean.money","X61_mean.money","X62_mean.money","X63_mean.money","X64_mean.money")


data<-cbind(data,Xmeanmoney)
head(data)



#생명보험/손해보험/제3보험으로 분류하기
#개수
#생명보험합
data$sum_1<-data$X76

#손해보험합
data$sum_2<-data$X65+data$X66+data$X67+data$X68+data$X69+data$X70+data$X71+data$X72+data$X73+data$X74+data$X75+data$X80+data$X81+data$X82+data$X83+data$X84+data$X85

#제3보험업합
data$sum_3<-data$X77+data$X78+data$X79

head(data)


#보험별로 묶고 보험료 합계
 
#생명보험합계
data$sum_money1<-data$X55_money 

#손해보험합계 
data$sum_money2<-data$X44_money+data$X45_money+data$X46_money+data$X47_money+data$X48_money+data$X49_money+data$X50_money+data$X51_money+data$X52_money+data$X53_money+data$X54_money+data$X59_money+data$X60_money+data$X61_money+data$X62_money+data$X63_money+data$X64_money

#제3보험업
data$sum_money3<-data$X56_money+data$X57_money+data$X58_money

#보헙별로 보헙평균값구하기
data$mean_money1<-data$sum_money1/data$sum_1
data$mean_money2<-data$sum_money2/data$sum_2
data$mean_money3<-data$sum_money3/data$sum_3


table(data$sum_2)

#sum값이 0인 경우-NaN를0으로 바꿔줌

data$X44_mean<-replace(data$X44_mean,data$X44_mean=="NaN",0)
data$X44_mean.money<-replace(data$X44_mean,data$X44_mean=="NaN",0)
data$X46_mean.money<-replace(data$X46_mean,data$X46_mean=="NaN",0)
data$X47_mean.money<-replace(data$X47_mean,data$X47_mean=="NaN",0)
data$X49_mean.money<-replace(data$X49_mean,data$X49_mean=="NaN",0)
data$X54_mean.money<-replace(data$X54_mean,data$X54_mean=="NaN",0)
data$X57_mean.money<-replace(data$X57_mean,data$X57_mean=="NaN",0)
data$X59_mean.money<-replace(data$X59_mean,data$X59_mean=="NaN",0)
data$X64_mean.money<-replace(data$X64_mean,data$X64_mean=="NaN",0)

data$mean_money1<-replace(data$mean_money1,data$mean_money1=="NaN",0)

data$mean_money2<-replace(data$mean_money2,data$mean_money2=="NaN",0)

data$mean_money3<-replace(data$mean_money3,data$mean_money3=="NaN",0)

head(data)

#X1,X5 factor처리하기
data$X1<-as.factor(data$X1)
data$X5<-as.factor(data$X5)

head(data)
library(dplyr)

str(data)
data1<-data[,c(1,2,3,4,5,9,10,15,19:24,31,35,43,65:155)]
str(data)
length(data[1,])

#glm값 하나하나 나열해서 aic값을 기준으로 나열

glm1<-glm(X86~X1,data=data1,family=binomial)
glm2<-glm(X86~X2,data=data1,family=binomial)
glm3<-glm(X86~X3,data=data1,family=binomial)
glm4<-glm(X86~X4,data=data1,family=binomial)
glm5<-glm(X86~X5,data=data1,family=binomial)
glm9<-glm(X86~X9,data=data1,family=binomial)
glm10<-glm(X86~X10,data=data1,family=binomial)
glm15<-glm(X86~X15,data=data1,family=binomial)
glm19<-glm(X86~X19,data=data1,family=binomial)
glm20<-glm(X86~X20,data=data1,family=binomial)
glm21<-glm(X86~X21,data=data1,family=binomial)
glm22<-glm(X86~X22,data=data1,family=binomial)
glm23<-glm(X86~X23,data=data1,family=binomial)
glm24<-glm(X86~X24,data=data1,family=binomial)
glm31<-glm(X86~X31,data=data1,family=binomial)
glm35<-glm(X86~X35,data=data1,family=binomial)
glm43<-glm(X86~X43,data=data1,family=binomial)
glm65<-glm(X86~X65,data=data1,family=binomial)
glm66<-glm(X86~X66,data=data1,family=binomial)
glm67<-glm(X86~X67,data=data1,family=binomial)
glm68<-glm(X86~X68,data=data1,family=binomial)
glm69<-glm(X86~X69,data=data1,family=binomial)
glm70<-glm(X86~X70,data=data1,family=binomial)
glm71<-glm(X86~X71,data=data1,family=binomial)
glm72<-glm(X86~X72,data=data1,family=binomial)
glm73<-glm(X86~X73,data=data1,family=binomial)
glm74<-glm(X86~X74,data=data1,family=binomial)
glm75<-glm(X86~X75,data=data1,family=binomial)
glm76<-glm(X86~X76,data=data1,family=binomial)
glm77<-glm(X86~X77,data=data1,family=binomial)
glm78<-glm(X86~X78,data=data1,family=binomial)
glm79<-glm(X86~X79,data=data1,family=binomial)
glm80<-glm(X86~X80,data=data1,family=binomial)
glm81<-glm(X86~X81,data=data1,family=binomial)
glm82<-glm(X86~X82,data=data1,family=binomial)
glm83<-glm(X86~X83,data=data1,family=binomial)
glm84<-glm(X86~X84,data=data1,family=binomial)
glm85<-glm(X86~X85,data=data1,family=binomial)
glm4_1<-glm(X86~X4_1,data=data1,family=binomial)
glm9_1<-glm(X86~X9_1,data=data1,family=binomial)
glm10_1<-glm(X86~X10_1,data=data1,family=binomial)
glm15_1<-glm(X86~X15_1,data=data1,family=binomial)
glm16_1<-glm(X86~X16_1,data=data1,family=binomial)
glm19_1<-glm(X86~X19_1,data=data1,family=binomial)
glm20_1<-glm(X86~X20_1,data=data1,family=binomial)
glm21_1<-glm(X86~X21_1,data=data1,family=binomial)
glm22_1<-glm(X86~X22_1,data=data1,family=binomial)
glm23_1<-glm(X86~X23_1,data=data1,family=binomial)
glm24_1<-glm(X86~X24_1,data=data1,family=binomial)
glm31_1<-glm(X86~X31_1,data=data1,family=binomial)
glm32_1<-glm(X86~X32_1,data=data1,family=binomial)
glm35_1<-glm(X86~X35_1,data=data1,family=binomial)
glm37_1<-glm(X86~X37_1,data=data1,family=binomial)
glm38_1<-glm(X86~X38_1,data=data1,family=binomial)
glm44_mean<-glm(X86~X44_mean,data=data1,family=binomial)
glm42_1<-glm(X86~X42_1,data=data1,family=binomial)
glm44_money<-glm(X86~X44_money,data=data1,family=binomial)
glm45_money<-glm(X86~X45_money,data=data1,family=binomial)
glm46_money<-glm(X86~X46_money,data=data1,family=binomial)
glm47_money<-glm(X86~X47_money,data=data1,family=binomial)
glm48_money<-glm(X86~X48_money,data=data1,family=binomial)
glm49_money<-glm(X86~X49_money,data=data1,family=binomial)
glm50_money<-glm(X86~X50_money,data=data1,family=binomial)
glm51_money<-glm(X86~X51_money,data=data1,family=binomial)
glm52_money<-glm(X86~X52_money,data=data1,family=binomial)
glm53_money<-glm(X86~X53_money,data=data1,family=binomial)
glm54_money<-glm(X86~X54_money,data=data1,family=binomial)
glm55_money<-glm(X86~X55_money,data=data1,family=binomial)
glm56_money<-glm(X86~X56_money,data=data1,family=binomial)
glm57_money<-glm(X86~X57_money,data=data1,family=binomial)
glm58_money<-glm(X86~X58_money,data=data1,family=binomial)
glm59_money<-glm(X86~X59_money,data=data1,family=binomial)
glm60_money<-glm(X86~X60_money,data=data1,family=binomial)
glm61_money<-glm(X86~X61_money,data=data1,family=binomial)
glm62_money<-glm(X86~X62_money,data=data1,family=binomial)
glm63_money<-glm(X86~X63_money,data=data1,family=binomial)
glm64_money<-glm(X86~X64_money,data=data1,family=binomial)

glm44_mean.money<-glm(X86~X44_mean.money,data=data1,family=binomial)
glm45_mean.money<-glm(X86~X45_mean.money,data=data1,family=binomial)
glm46_mean.money<-glm(X86~X46_mean.money,data=data1,family=binomial)
glm47_mean.money<-glm(X86~X47_mean.money,data=data1,family=binomial)
glm48_mean.money<-glm(X86~X48_mean.money,data=data1,family=binomial)
glm49_mean.money<-glm(X86~X49_mean.money,data=data1,family=binomial)
glm50_mean.money<-glm(X86~X50_mean.money,data=data1,family=binomial)
glm51_mean.money<-glm(X86~X51_mean.money,data=data1,family=binomial)
glm52_mean.money<-glm(X86~X52_mean.money,data=data1,family=binomial)
glm53_mean.money<-glm(X86~X53_mean.money,data=data1,family=binomial)
glm54_mean.money<-glm(X86~X54_mean.money,data=data1,family=binomial)
glm55_mean.money<-glm(X86~X55_mean.money,data=data1,family=binomial)
glm56_mean.money<-glm(X86~X56_mean.money,data=data1,family=binomial)
glm57_mean.money<-glm(X86~X57_mean.money,data=data1,family=binomial)
glm58_mean.money<-glm(X86~X58_mean.money,data=data1,family=binomial)
glm59_mean.money<-glm(X86~X59_mean.money,data=data1,family=binomial)
glm60_mean.money<-glm(X86~X60_mean.money,data=data1,family=binomial)
glm61_mean.money<-glm(X86~X61_mean.money,data=data1,family=binomial)
glm62_mean.money<-glm(X86~X62_mean.money,data=data1,family=binomial)
glm63_mean.money<-glm(X86~X63_mean.money,data=data1,family=binomial)
glm64_mean.money<-glm(X86~X64_mean.money,data=data1,family=binomial)
glmsum_1<-glm(X86~ sum_1,data=data1,family=binomial)
glmsum_2<-glm(X86~ sum_2,data=data1,family=binomial)
glmsum_3<-glm(X86~ sum_3,data=data1,family=binomial)
glmsum_money1<-glm(X86~ sum_money1,data=data1,family=binomial)
glmsum_money2<-glm(X86~ sum_money2,data=data1,family=binomial)
glmsum_money3<-glm(X86~ sum_money3,data=data1,family=binomial)
glmmean_money1<-glm(X86~mean_money1,data=data1,family=binomial)
glmmean_money2<-glm(X86~mean_money2,data=data1,family=binomial)
glmmean_money3<-glm(X86~mean_money3,data=data1,family=binomial)

ncol(data1)
colnames(data1)

a<-AIC(glm1,glm2,glm3,glm4,glm5,glm9,glm10,glm15,glm19,glm20,glm21
,glm22,glm23,glm24,glm31,glm35,glm43,glm65,glm66,glm67,glm68
,glm69,glm70,glm71,glm72,glm73,glm74,glm75,glm76,glm77,glm78
,glm79,glm80,glm81,glm82,glm83,glm84,glm85,glm4_1,glm9_1,glm10_1
,glm15_1,glm16_1,glm19_1,glm20_1,glm21_1,glm22_1,glm23_1,glm24_1
,glm31_1,glm32_1,glm35_1,glm37_1,glm38_1,glm44_mean,glm42_1
,glm44_money,glm45_money,glm46_money,glm47_money,glm48_money
,glm49_money,glm50_money,glm51_money,glm52_money,glm53_money
,glm54_money,glm55_money,glm56_money,glm57_money,glm58_money
,glm59_money,glm60_money,glm61_money,glm62_money,glm63_money
,glm64_money
,glm44_mean.money,glm45_mean.money,glm46_mean.money,glm47_mean.money
,glm48_mean.money,glm49_mean.money,glm50_mean.money,glm51_mean.money
,glm52_mean.money,glm53_mean.money,glm54_mean.money,glm55_mean.money
,glm56_mean.money,glm57_mean.money,glm58_mean.money,glm59_mean.money
,glm60_mean.money,glm61_mean.money,glm62_mean.money,glm63_mean.money
,glm64_mean.money,glmsum_1,glmsum_2,glmsum_3,
glmsum_money1,glmsum_money2,glmsum_money3,
glmmean_money1,glmmean_money2,glmmean_money3)

arrange(a,AIC)

#필요한변수들만 추출해서 aic값 보기

e<-AIC(glm2,glm3,glm4,glm5,glm10,glm19,glm20,glm21
,glm22,glm23,glm24,glm35,glm43,glm65,glm66,glm67,glm68
,glm69,glm70,glm71,glm72,glm73,glm74,glm75,glm76,glm77,glm78
,glm79,glm80,glm81,glm82,glm83,glm84,glm85,glm4_1,glm9_1
,glm15_1,glm16_1,glm31_1,glm32_1,glm37_1,glm44_mean,glm42_1
,glm44_money,glm46_money,glm47_money,glm49_money
,glm54_money,glm57_money,glm59_money,glm64_money
,glm44_mean.money,glm46_mean.money,glm47_mean.money,glm49_mean.money
,glm54_mean.money,glm57_mean.money,glm59_mean.money,glm64_mean.money
,glmsum_1,glmsum_2,glmsum_3
,glmsum_money2,glmmean_money2)

colnames(data2)

#data1에서 필요한 변수만 추출해서 data2에 지정

data2<-data1[,c(2,3,4,5,7,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25
                ,26:39,40,41,43,44,51,52,54,56,57,58,60,61,63,68,71,73,78,
                 79,81,82,84,89,92,94,99,100,101,102,104,107)]

colnames(data1)
ncol(data2)

#로지스틱돌리기

model_5<-glm(X86~.,data=data2,family=binomial)
summary(model_5)

#forward 코드?
model_6<-step(model_5,direction="backward")
model_7<-step(model_5,direction="both")

#3가지방법으로 돌린 것 합집합

model_8<-glm(X86 ~ X5 + X10 + X20 + X21 + X22 + X23 + X65 + 
    X68 + X69 + X70 + X73 + X75 + X9_1 + X16_1 + X44_mean + X44_money + 
    X46_money + X47_money + X57_money + X44_mean.money + X47_mean.money + 
    sum_money2 + mean_money2 + X82 + X2 + X24 + X85+ X59_money,data=data2,family=binomial)

model_fin<-step(model_8,direction="both")



#최종
Step:  AIC=1385.85
X86 ~ X5 + X10 + X20 + X21 + X22 + X23 + X65 + X68 + X69 + X70 + 
    X73 + X75 + X9_1 + X16_1 + X44_mean + X44_money + X46_money + 
    X47_money + X57_money + X44_mean.money + X47_mean.money + 
    sum_money2 + mean_money2 + X82

data3<-data2[,c(4,5,7,8,9,10,14,17,18,19,22,24,31,35,37,39,43,45,46,47,50,53,55,64,65)]
colnames(data2)

data2_1<-data1[,c(21,101,82,61,104,56,79,58,18,1,5,44,11,17,33,54,57,51,7,14,39)]
colnames(data3_1)
data3_1<-data2_1[,c(1,3,4,6,9,12,13,15,19,20,11,21)]



############################################
###############인공신경망모델링################
############################################
data3<-data2[,c(4,5,7,8,9,10,14,17,18,19,22,24,31,35,37,39,43,45,46,47,50,53,55,64,65)]


#신경망은 정규화과정 필요
normalize<-function(x) {
 return((x-min(x))/(max(x)-min(x)))
}

#전체데이터프레임에 정규화적용
data4<-data3[,-c(1,14)]
data5<-data3[,c(1,14)]

#변수더 축소했을떄 사용
data4<-data3_1[,-c(11,12)]
data5<-data3_1[,c(11,12)]



data4_norm<-as.data.frame(lapply(data4,normalize))
data_result<-cbind(data4,data5)
str(data_result)

library(caret)
set.seed(1149)	

rn<-createDataPartition(y=data_result$X86,p=0.7,list=F)
train<-data_result[rn,]
test<-data_result[-rn,]

table(train$X86)
table(test$X86)



#nnet활용
#install.packagws("nnet")
library(nnet)

nn.model<- nnet(X86~.,data=train, size=11, decay = 5e-04)  #원래defualt 0인데, 이게더정확
summary(nn.model)
pred<-predict(nn.model,test,type="class")	#350개의예측값


#예측값확인 
actual<-test$X86
table(actual, pred)

test=cbind(test,pred)
test$pred<-ifelse(test$X86==test$pred,1,0)	#맞게예측하면1출력
table(test$pred)	
sum(test$pred)/350



#신경망모형 정오분류표
table(data$X86, predict(nn.model, data2, type="class"))

#예측값과 실제값 상관관계확인(1에 가까워야함)
cor(predict, test$X86)




#####반복문사용#########
ma<-matrix(0,100,1)

for (i in 1:100){
set.seed(i)	

rn<-createDataPartition(y=data_result$X86,p=0.7,list=F)
train<-data_result[rn,]
test<-data_result[-rn,]

library(nnet)

nn.model<- nnet(X86~.,data=train, size=5, decay = 5e-04)  #원래defualt 0인데, 이게더정확
pred<-predict(nn.model,test,type="class")	#350개의예측값
 
actual<-test$X86

test=cbind(test,pred)
test$pred<-ifelse(test$X86==test$pred,1,0)	#맞게예측하면1출력

ma[i,]<-sum(test$pred)/350
i=i+1
}

ma






#Factor라 neuralnet사용불가#아래코드사용X##
#####Training a model on the data#####
######################################
install.packages("neuralnet")
library(neuralnet)

data_model<-neuralnet(X86~., data=train)
plot(data_model)

model_result<-compute(data_model,test[1:25])

predict<-model_result$net.result

#예측값과 실제값 상관관계확인(1에 가까워야함)
cor(predict, data_test$X86)


#hidden layer늘린모델확인
data_mode2<-neuralnet(X86~., data=train, hidden=5)
model_result<-compute(data_model2,test[1:25])
predict2<-model_result2$net.result
cor(predict2, data_test$X86)



###############의사결정트리####################
str(data3_1)
set.seed(3)
library(caret)
in.train <- createDataPartition(data3$X86, p=0.7, list=FALSE)
train<-data3_1[in.train,]
test<-data3_1[-in.train,]

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

deep<-rpart(X86~.,data=train,method="class",minsplit=15)
rpart.plot(deep)

test_pred<-predict(deep,test,type='class')
sum(test$X86==test_pred)/nrow(test)
]

################################################
################################################

head(data)

나이
집소유여부
직업
교육수준
결혼유무

data$married_fee=(data$X10*1370+(data$X11+data$X12)*1480)/(data$X10+data$X11+data$X12)
data$edu_fee=(data$X16*1475+data$X17*1510+data$X18*1525)/(data$X16+data$X17+data$X18)
data$home_fee=(data$X30*1483+data$X31*1452)/(data$X30+data$X31)
data$age_fee=0
for (i in 1:length(data$X1)) {
if (data$X4[i]==1){
data$age_fee[i]=1887
} else if(data$X4[i]==2){
data$age_fee[i]=1449
} else if(data$X4[i]==3){
data$age_fee[i]=1394
} else if (data$X4[i]==4){
data$age_fee[i]=1298
} else if (data$X4[i]==5){
data$age_fee[i]=1315
} else if (data$X4[i]==6){
data$age_fee[i]=1523
}
}
str(data$X4)
data$X4
hist(data$avg_fee)

data$avg_fee=(data$married_fee+data$edu_fee+data$home_fee+data$age_fee)/4

colnames(data)



##############################################################################
data1_2<-data[,c(1,2,3,4,5,9,10,15,19:24,31,35,43,65:155,160)]
data2_1<-data1_2[,c(2,3,4,5,7,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25
                ,26:39,40,41,43,44,51,52,54,56,57,58,60,61,63,68,71,73,78,
                 79,81,82,84,89,92,94,99,100,101,102,104,107,109)]


#인공신경망에만 넣어보기

data2_2<-data1_2[,c(21,101,82,61,104,56,79,58,18,1,5,44,11,17,33,54,57,51,7,14,39,109)]
data3_2<-data2_2[,c(1,3,4,6,9,12,13,15,19,20,11,21,22)]

colnames(data3_2)


#신경망은 정규화과정 필요
normalize<-function(x) {
 return((x-min(x))/(max(x)-min(x)))
}


#변수더 축소했을떄 사용
data4_2<-data3_2[,-c(11,12)]
data5_2<-data3_2[,c(11,12)]



data4_norm<-as.data.frame(lapply(data4_2,normalize))
data_result2<-cbind(data4_2,data5_2)
str(data_result2)

library(caret)
set.seed(1149)	#노드수11개, 0.70

rn2<-createDataPartition(y=data_result2$X86,p=0.7,list=F)
train2<-data_result2[rn2,]
test2<-data_result2[-rn2,]



#nnet활용
#install.packagws("nnet")
library(nnet)

nn.model2<- nnet(X86~.,data=train2, size=11, decay = 5e-04)  #원래defualt 0인데, 이게더정확
summary(nn.model2)
pred2<-predict(nn.model2,test2,type="class")	#350개의예측값


#예측값확인 
actual2<-test$X86
table(actual2, pred2)

test2=cbind(test2,pred2)
test2$pred2<-ifelse(test2$X86==test2$pred2,1,0)	#맞게예측하면1출력
table(test2$pred2)	
sum(test2$pred2)/350


#####반복문사용#########
ma2<-matrix(0,100,1)

for (i in 1:100){
set.seed(i)	

rn2<-createDataPartition(y=data_result2$X86,p=0.7,list=F)
train2<-data_result2[rn2,]
test2<-data_result2[-rn2,]

library(nnet)

nn.model2<- nnet(X86~.,data=train2, size=5, decay = 5e-04)  #원래defualt 0인데, 이게더정확
pred2<-predict(nn.model2,test2,type="class")	#350개의예측값
 
actual2<-test2$X86

test2=cbind(test2,pred2)
test2$pred2<-ifelse(test2$X86==test2$pred2,1,0)	#맞게예측하면1출력

ma2[i,]<-sum(test2$pred2)/350
i=i+1
}

ma
ma2



colnames(data3_2)






################################
################################
#edu
fee=matrix(0,length(data[,1]),3)
i=16
for (t in 1:3) {
for (j in 1:length(data[,1])){
if (data[j,i]==0){
fee[j,t]=0
}
else if (data[j,i]==1) {
fee[j,t]=0.01
}
else if (data[j,i]==2) {
fee[j,t]=0.11
}
else if (data[j,i]==3) {
fee[j,t]=0.24
}
else if (data[j,i]==4) {
fee[j,t]=0.37
}
else if (data[j,i]==5) {
fee[j,t]=0.50
}
else if (data[j,i]==6) {
fee[j,t]=0.63
}
else if (data[j,i]==7) {
fee[j,t]=0.76
}
else if (data[j,i]==8) {
fee[j,t]=0.89
}
else{
fee[j,t]=1
}
}
i=i+1
}

fee<-data.frame(fee)

names(fee)<-c("low","medium","high")
data<-cbind(data,fee)
str(data)
head(data[,c(16:18,156:158)])

#house
fee_1=matrix(0,length(data[,1]),2)
i=30
for (t in 1:2) {
for (j in 1:length(data[,1])){
if (data[j,i]==0){
fee_1[j,t]=0
}
else if (data[j,i]==1) {
fee_1[j,t]=0.01
}
else if (data[j,i]==2) {
fee_1[j,t]=0.11
}
else if (data[j,i]==3) {
fee_1[j,t]=0.24
}
else if (data[j,i]==4) {
fee_1[j,t]=0.37
}
else if (data[j,i]==5) {
fee_1[j,t]=0.50
}
else if (data[j,i]==6) {
fee_1[j,t]=0.63
}
else if (data[j,i]==7) {
fee_1[j,t]=0.76
}
else if (data[j,i]==8) {
fee_1[j,t]=0.89
}
else{
fee_1[j,t]=1
}
}
i=i+1
}

fee_1<-data.frame(fee_1)

names(fee_1)<-c("rented","owned")
summary(fee_1)
data<-cbind(data,fee_1)
str(data)
head(data[,c(30,31,159,160)])

fee_marriage<-data$X10_1*1480+(1-data$X10_1)*1370
fee_edu<-data$low*1525+data$medium*1510+data$high*1475
fee_house<-data$rented*1483+data$owned*1452
fee_age<-ifelse(data$X4==1,1887,
	ifelse(data$X4==2,1449,
	ifelse(data$X4==3,1394,
	ifelse(data$X4==4,1298,
	ifelse(data$X4==5,1315,
	ifelse(data$X4==6,1523,0)))
	)))
	
data$avg_fee<-matrix(0,nrow=9822)
data$avg_fee<-(fee_marriage+fee_edu+fee_house+fee_age)/4
summary(data$avg_fee)



cbind(ma,ma2)





###################################################
###################보고서수정#######################
###################################################
###보험료 넣기전 최종변수####
colnames(data3_1)
data3_1<-data2_1[,c(1,3,4,6,9,12,13,15,19,20,11,21)]

################################################
#################로지스틱##########################
#################################################
#보험료 없는거

in.train_1 <- createDataPartition(data3_1$X86, p=0.7, list=FALSE)
train_1<-data3_1[in.train_1,]
test_1<-data3_1[-in.train_1,]
table(train_1$X86)
table(test_1$X86)

model_1<-glm(X86~.,data=train_1,family=binomial)
pre_1 <- predict(model_1, newdata=test_1, type="response")

x<-table(round(pre_1),test_1$X86)
x
print(x[1,1]+x[2,2])


confusionMatrix(as.factor(round(pre_1)),as.factor(test_1$X86))


#################의사결정트리#######################
##############보험료 포함#######맨뒤에코드존재########
#################################################
colnames(data3_1)
colnames(data4_1)
colnames(data)
data4_1<-data[,c(68,129,108,103,65,91,21,80,10,24,5,86,161)]

#rpart함수 수행과 그래프 그리기를 위한 패키지 불러오기
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

set.seed(13)
library(caret)
in.train <- createDataPartition(data4_1$X86, p=0.7, list=FALSE)
train<-data4_1[in.train,]
test<-data4_1[-in.train,]

#rpart를 이용한 의사결정나무
a<-rpart.control(maxdepth=20)
deep<-rpart(X86~.,data=train,control=a,method="class")
rpart.plot(deep)

#가지치기를 위해 적절한 terminal node 개수 확인
printcp(deep)
plotcp(deep)

#가지치기를 수행한 결과 출력
tree<-prune(deep, cp=deep$cptable[which.min(deep$cptable[,"xerror"]),"CP"])
rpart.plot(tree)

#test데이터 셋을 이용한 정확도 평가
test_pred<-predict(tree,test,type='class')
sum(test$X86==test_pred)/nrow(test)

confusionMatrix(test_pred,as.factor(test$X86))


##################보험료제외(완)

set.seed(13)
library(caret)
in.train <- createDataPartition(data3_1$X86, p=0.7, list=FALSE)
train<-data3_1[in.train,]
test<-data3_1[-in.train,]

#rpart를 이용한 의사결정나무
a<-rpart.control(maxdepth=20)
deep<-rpart(X86~.,data=train,control=a,method="class")
rpart.plot(deep)

#가지치기를 위해 적절한 terminal node 개수 확인
printcp(deep)
plotcp(deep)

#가지치기를 수행한 결과 출력
tree<-prune(deep, cp=deep$cptable[which.min(deep$cptable[,"xerror"]),"CP"])
rpart.plot(tree)

#test데이터 셋을 이용한 정확도 평가
test_pred<-predict(tree,test,type='class')
sum(test$X86==test_pred)/nrow(test)


confusionMatrix(test_pred,as.factor(test$X86))




#################XGBOOST#######################
library(Matrix)
#install.packages("xgboost")
library(xgboost)

#xgboost 적용하기
train$X86 <- as.numeric(train$X86)-1 
  #Xgboost는 nuemric 변수만 적용 가능하며 0 또는 1 을 label 값으로 한다.
table(train$X86)
test$X86 <- as.numeric(test$X86)-1
table(test$X86)
prop.table(table(test$X86))

str(train)


params<-list(
   eta = 0.15,
   max.depth = 5,
   objective = "binary:logistic",
   eval_metric = "error")

xgbcv <- xgb.cv(param = params,
           data = as.matrix(train[,-11]), #입력변수, 반드시 숫자여야함
                    label = train$X86, #결과변수, 반드시 숫자 0 또는 1이어야 함
                    nrounds = 300, #모델을 얼마나 생성할지
                    colsample_bytree = 0.9, #변수 선택 비율 일반적으로 0.5 ~ 1
           nfold = 20,
           verbose = 1,
           print_every_n = 25,
           early_stopping_rounds = 100) 

databoost <- xgboost(param = params,
           data = as.matrix(train[,-11]), #입력변수, 반드시 숫자여야함
                    label = train$X86, #결과변수, 반드시 숫자 0 또는 1이어야 함
                    nrounds = xgbcv$best_iteration, #모델을 얼마나 생성할지
                    colsample_bytree = 0.9, #변수 선택 비율 일반적으로 0.5 ~ 1
           verbose=0,
           early_stopping_rounds = 100) 

boost_pred<-predict(databoost, as.matrix(test[,-11]))

boost_pred<-ifelse(boost_pred > 0.5 , 1, 0)
confusionMatrix(as.factor(boost_pred), as.factor(test$X86))


#######################인공신경망###############################
data3<-data2[,c(4,5,7,8,9,10,14,17,18,19,22,24,31,35,37,39,43,45,46,47,50,53,55,64,65)]


#신경망은 정규화과정 필요
normalize<-function(x) {
 return((x-min(x))/(max(x)-min(x)))
}

#전체데이터프레임에 정규화적용
data4<-data3[,-c(1,14)]
data5<-data3[,c(1,14)]

#변수더 축소했을떄 사용
data4<-data3_1[,-c(11,12)]
data5<-data3_1[,c(11,12)]



data4_norm<-as.data.frame(lapply(data4,normalize))
data_result<-cbind(data4,data5)
str(data_result)

library(caret)
set.seed(1149)	

rn<-createDataPartition(y=data_result$X86,p=0.7,list=F)
train<-data_result[rn,]
test<-data_result[-rn,]

table(train$X86)
table(test$X86)



#nnet활용
#install.packagws("nnet")
library(nnet)

nn.model<- nnet(X86~.,data=train, size=11, decay = 5e-04)  #원래defualt 0인데, 이게더정확
summary(nn.model)
pred<-predict(nn.model,test,type="class")	#350개의예측값


#예측값확인 
actual<-test$X86
table(actual, pred)

test=cbind(test,pred)
test$pred<-ifelse(test$X86==test$pred,1,0)	#맞게예측하면1출력
table(test$pred)	
sum(test$pred)/350


#신경망모형 정오분류표
table(data$X86, predict(nn.model, data2, type="class"))

confusionMatrix(as.factor(pred), as.factor(test$X86))





####################################################
#####################보험료변수추가####################
#####################################################
#edu
fee=matrix(0,length(data[,1]),3)
i=16
for (t in 1:3) {
for (j in 1:length(data[,1])){
if (data[j,i]==0){
fee[j,t]=0
}
else if (data[j,i]==1) {
fee[j,t]=0.01
}
else if (data[j,i]==2) {
fee[j,t]=0.11
}
else if (data[j,i]==3) {
fee[j,t]=0.24
}
else if (data[j,i]==4) {
fee[j,t]=0.37
}
else if (data[j,i]==5) {
fee[j,t]=0.50
}
else if (data[j,i]==6) {
fee[j,t]=0.63
}
else if (data[j,i]==7) {
fee[j,t]=0.76
}
else if (data[j,i]==8) {
fee[j,t]=0.89
}
else{
fee[j,t]=1
}
}
i=i+1
}

fee<-data.frame(fee)

names(fee)<-c("low","medium","high")
data<-cbind(data,fee)
str(data)
head(data[,c(16:18,156:158)])

#house
fee_1=matrix(0,length(data[,1]),2)
i=30
for (t in 1:2) {
for (j in 1:length(data[,1])){
if (data[j,i]==0){
fee_1[j,t]=0
}
else if (data[j,i]==1) {
fee_1[j,t]=0.01
}
else if (data[j,i]==2) {
fee_1[j,t]=0.11
}
else if (data[j,i]==3) {
fee_1[j,t]=0.24
}
else if (data[j,i]==4) {
fee_1[j,t]=0.37
}
else if (data[j,i]==5) {
fee_1[j,t]=0.50
}
else if (data[j,i]==6) {
fee_1[j,t]=0.63
}
else if (data[j,i]==7) {
fee_1[j,t]=0.76
}
else if (data[j,i]==8) {
fee_1[j,t]=0.89
}
else{
fee_1[j,t]=1
}
}
i=i+1
}

fee_1<-data.frame(fee_1)

names(fee_1)<-c("rented","owned")
summary(fee_1)
data<-cbind(data,fee_1)
str(data)
head(data[,c(30,31,159,160)])

fee_marriage<-data$X10_1*1480+(1-data$X10_1)*1370
fee_edu<-data$low*1525+data$medium*1510+data$high*1475
fee_house<-data$rented*1483+data$owned*1452
fee_age<-ifelse(data$X4==1,1887,
	ifelse(data$X4==2,1449,
	ifelse(data$X4==3,1394,
	ifelse(data$X4==4,1298,
	ifelse(data$X4==5,1315,
	ifelse(data$X4==6,1523,0)))
	)))
	
data$avg_fee<-matrix(0,nrow=9822)
data$avg_fee<-(fee_marriage+fee_edu+fee_house+fee_age)/4
summary(data$avg_fee)













