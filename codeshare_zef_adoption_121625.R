library(tidyverse)
library(readxl)
library(lavaan)
library(moments)

mydata=read_csv("data_zef_adoption.csv")

#to see what variables have missing cases
mydata %>% count(use_intention) # no missing values
mydata %>% count(fatigue) %>% print(n=Inf) #no missing values 
mydata %>% count(BI_Facial_1) #1919 NA
mydata %>% count(UTAUT1_PE_2,UTAUT1_EE_2) %>% print(n=Inf) #1224 NA

#April, sample number re-examine
sample1<-mydata %>% filter(!is.na(fatigue)&!is.na(facial)&!is.na(im.manage)&!is.na(Time_Video))
sample1 #model1 (facial, zef, impression manage), 453
sample1 %>% count(GenderIdentity)

sample2<-mydata %>% filter(!is.na(fatigue)&!is.na(facial)&!is.na(Time_Video)&!is.na(UTAUT1_PE_1))
sample2 #Model2, (facial, zef, TAM), 267 
sample2 %>% count(GenderIdentity)
sample2 %>% count(im.manage) #39 were excluded 

mydata %>% filter(!is.na(facial)&!is.na(im.manage)&!is.na(UTAUT1_PE_1)&!is.na(UTAUT1_EE_1))

#skewness and kurtosis testing
testing_normal=mydata %>% filter(!is.na(im.manage)&!is.na(BI_Facial_1)&!is.na(PE))

skewness(testing_normal$use_intention) #-0.68 |2| normal
kurtosis(testing_normal$use_intention) #3.29 |7| normal 
skewness(testing_normal$fatigue)
kurtosis(testing_normal$fatigue)
skewness(testing_normal$im.manage)
kurtosis(testing_normal$im.manage)
skewness(testing_normal$PE)
kurtosis(testing_normal$PE)
skewness(testing_normal$EE)
kurtosis(testing_normal$EE)

#correlations
mydata %>% select(GenderIdentity,fatigue,Time_Video,PE,EE) %>% as.matrix(.) %>% Hmisc::rcorr(.)
testing_normal %>% select(GenderIdentity,YearsExp,Time_Video,im.manage,facial,fatigue,PE,EE,use_intention) %>% as.matrix(.) %>% Hmisc::rcorr(.)
testing_normal %>% count(GenderIdentity)

#commom method bias testing
testing_normal %>% select(PF_Self_3:PF_Self_5,PF_Self_8,UseAttAndIntent_4:UseAttAndIntent_6,UTAUT1_PE_1:UTAUT1_PE_3,UTAUT1_EE_1:UTAUT1_EE_3,
                          PlatFatigue_1:PlatFatigue_15,BI_Facial_1:BI_Facial_4) %>% psych::fa(.,nfactors=1,rotate="promax")

#reliabiliry (cronbach's alpha) 
testing_normal %>% select(PF_Self_3:PF_Self_5,PF_Self_8) %>% psych::alpha()
testing_normal %>% select(BI_Facial_1:BI_Facial_4) %>% psych::alpha()
testing_normal %>% select(PlatFatigue_1:PlatFatigue_15) %>% psych::alpha()
testing_normal %>% select(UTAUT1_PE_1:UTAUT1_PE_3) %>% psych::alpha()
testing_normal %>% select(UTAUT1_EE_1:UTAUT1_EE_3) %>% psych::alpha()
testing_normal %>% select(UseAttAndIntent_4:UseAttAndIntent_6) %>% psych::alpha()

#descriptive of each variable
testing_normal %>% summarize(format(mean(facial)))
testing_normal %>% summarize(format(sd(facial)))
testing_normal %>% summarize(format(mean(im.manage)))
testing_normal %>% summarize(format(sd(im.manage)))
testing_normal %>% summarize(format(mean(fatigue)))
testing_normal %>% summarize(format(sd(fatigue)))
testing_normal %>% summarize(format(mean(PE)))
testing_normal %>% summarize(format(sd(PE)))
testing_normal %>% summarize(format(mean(EE)))
testing_normal %>% summarize(format(sd(EE)))
testing_normal %>% summarize(format(mean(use_intention)))
testing_normal %>% summarize(format(sd(use_intention)))

testing_normal %>% summarize(format(mean(Time_Video,na.rm=T)))
testing_normal %>% summarize(format(median(Time_Video,na.rm=T)))

testing_normal %>% summarize(format(mean(Time_Video)))
testing_normal %>% summarize(format(sd(Time_Video)))
testing_normal %>% summarize(median(Time_Video))

#descriptive of demographics
testing_normal %>% count(GenderIdentity)
testing_normal %>% summarize(range(Demo_Age))
testing_normal %>% summarize(format(mean(Demo_Age)))
testing_normal %>% summarize(format(sd(Demo_Age)))
testing_normal %>% summarize(range(YearsExp))
testing_normal %>% summarize(format(mean(YearsExp)))
testing_normal %>% summarize(format(sd(YearsExp)))

#median split for impression management usage
mydata %>% summarize(median(im.manage,na.rm=T))
mydata=mydata %>% mutate(
  impress_bi = ifelse(im.manage > 1.5, 1, 0)
)


#Model 1, reported: NEW mediation test (facial - fatigue - feat_impress) significant
m.model<-
  '
facecon=~BI_Facial_1+BI_Facial_2+BI_Facial_3+BI_Facial_4;

VMF=~PlatFatigue_1+PlatFatigue_2+PlatFatigue_3+PlatFatigue_4+PlatFatigue_5+PlatFatigue_6+PlatFatigue_7+
PlatFatigue_8+PlatFatigue_9+PlatFatigue_10+PlatFatigue_11+PlatFatigue_12+PlatFatigue_13+PlatFatigue_14+
PlatFatigue_15;

imp.manage=~PF_Self_3+PF_Self_4+PF_Self_5+PF_Self_8;


imp.manage~fi*VMF+facecon+Time_Video;

VMF~fv*facecon+Time_Video;

#comparison
med:=fv*fi;

'
test.med<-sem(m.model,data=mydata %>% filter(GenderIdentity==1|GenderIdentity==2))
summary(test.med,fit.measures=T,standardized=T,rsquare=T)

#Model 2 and Model 3. Reported
model4w<-
  '
facecon=~BI_Facial_1+BI_Facial_2+BI_Facial_3+BI_Facial_4;

VMF=~PlatFatigue_1+PlatFatigue_2+PlatFatigue_3+PlatFatigue_4+PlatFatigue_5+PlatFatigue_6+PlatFatigue_7+
PlatFatigue_8+PlatFatigue_9+PlatFatigue_10+PlatFatigue_11+PlatFatigue_12+PlatFatigue_13+PlatFatigue_14+
PlatFatigue_15;

use.inten=~UseAttAndIntent_4+UseAttAndIntent_5+UseAttAndIntent_6;

perfor=~UTAUT1_PE_1+UTAUT1_PE_2+UTAUT1_PE_3;
effort=~UTAUT1_EE_1+UTAUT1_EE_2+UTAUT1_EE_3;

use.inten~facecon+VMF+pi*perfor+ei*effort;

perfor~facecon+fp*VMF+ep*effort;
effort~facecon+fe*VMF;

VMF~cf*facecon+Time_Video;

#comparison
per.inten:=cf*fp*pi;
eff.inten:=cf*fe*ep*ei;
'

testset= mydata %>% filter(GenderIdentity==1|GenderIdentity==2) #create a subset only inclusive of binary gender identities
test.mod4.all<-sem(model4w,data=testset)
test.mod4.high<-sem(model4w,data=testset %>% filter(impress_bi==1))
test.mod4.low<-sem(model4w,data=testset %>% filter(impress_bi==0))
summary(test.mod4.all,fit.measures=T,standardized=T,rsquare=T)
high.im4<-summary(test.mod4.high,fit.measures=T,standardized=T,rsquare=T)
low.im4<-summary(test.mod4.low,fit.measures=T,standardized=T,rsquare=T)
high.im4$FIT
low.im4$FIT

#Model 3. Reported
model4<-
  '
facecon=~BI_Facial_1+BI_Facial_2+BI_Facial_3+BI_Facial_4;

VMF=~PlatFatigue_1+PlatFatigue_2+PlatFatigue_3+PlatFatigue_4+PlatFatigue_5+PlatFatigue_6+PlatFatigue_7+
PlatFatigue_8+PlatFatigue_9+PlatFatigue_10+PlatFatigue_11+PlatFatigue_12+PlatFatigue_13+PlatFatigue_14+
PlatFatigue_15;

use.inten=~UseAttAndIntent_4+UseAttAndIntent_5+UseAttAndIntent_6;

perfor=~UTAUT1_PE_1+UTAUT1_PE_2+UTAUT1_PE_3;
effort=~UTAUT1_EE_1+UTAUT1_EE_2+UTAUT1_EE_3;

use.inten~facecon+VMF+c(pi1,pi2)*perfor+c(ei1,ei2)*effort;

perfor~facecon+c(fp1,fp2)*VMF+c(ep1,ep2)*effort;
effort~facecon+c(fe1,fe2)*VMF;

VMF~c(cf1,cf2)*facecon+Time_Video;

#comparison
per.inten1:=cf1*fp1*pi1;
eff.inten1:=cf1*fe1*ep1*ei1;

per.inten2:=cf2*fp2*pi2;
eff.inten2:=cf2*fe2*ep2*ei2;

per.inten.dif:=per.inten1-per.inten2;
ef.inten.dif:=eff.inten1-eff.inten2;

cf.dif:=cf1-cf2;
fp.diff:=fp1-fp2;
fe.dif:=fe1-fe2;
ep.dif:=ep1-ep2;
pa.dif:=pi1-pi2;
ei.dif:=ei1-ei2;
'

test.mod4<-sem(model4,data=mydata %>% filter(GenderIdentity==1|GenderIdentity==2),group='impress_bi')
summary(test.mod4,fit.measures=T,standardized=T,rsquare=T)
