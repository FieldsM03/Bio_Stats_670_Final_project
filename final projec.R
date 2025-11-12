##Mitchell Fields##
##Final Project##

##librarys##
library(lme4)
library(lmerTest)
library(ggplot2)

##load in data##(1 = male; 0 = female)
heartdisease<-read.csv(file.choose(),header=TRUE)

#this shows that the ages for both females and males are close together. 
heart<-ggplot(heartdisease,aes(x=sex,y=age)) + geom_point()

##standard linear model
#linear model of serum cholesterol against age
chol_q_lm<-lm(chol~age,data = heartdisease)
summary(chol_q_lm)
anova(chol_q_lm)
###ggplot chol and age
chol_age_scatter<-ggplot(heartdisease,aes(x=heartdisease$age,y=heartdisease$chol)) + geom_point() + geom_smooth(method = "lm")

#linear model of serum cholesterol against age and maximum heart rate
chol_lm<-lm(chol~age+thalach,data = heartdisease)
summary(chol_lm) ##maximum is important with age
anova(chol_lm)
##ggplot 
chol_age+thalach_scatter<-ggplot(heartdisease,aes(x=heartdisease$age+thalach,y=heartdisease$chol)) + geom_point() + geom_smooth(method = "lm")


#linear model of serum cholesterol against age and resting blood pressure
chol_t_lm<-lm(chol~age+trestbps,data = heartdisease)
summary(chol_t_lm)#less important than
anova(chol_t_lm)
##ggplot 
chol_age+trestbps_scatter<-ggplot(heartdisease,aes(x=heartdisease$age+trestbps,y=heartdisease$chol)) + geom_point() + geom_smooth(method = "lm")


#linear model of serum cholesterol against age and resting blood pressure and maximum
#rate
chol_tt_lm<-lm(chol~age+trestbps+thalach,data = heartdisease)
summary(chol_tt_lm)
anova(chol_tt_lm)

#ggplot
chol_age+trestbps+thalach_scatter<-ggplot(heartdisease,aes(x=age+trestbps+thalach,y=chol)) + geom_point() + geom_smooth(method = "lm")



#this is bec both maximum heart rate and resting blood pressure has a r^2 over 0.05 
#this is bec both are correlation 
chol_ttw_lm<-lm(chol~trestbps+thalach,data = heartdisease)
summary(chol_ttw_lm)
anova(chol_ttw_lm)
#ggplot
chol_age+trestbps+thalach_scatter<-ggplot(heartdisease,aes(x=trestbps+thalach,y=chol)) + geom_point() + geom_smooth(method = "lm")


#ggplot resting blood
ggplot(heartdisease,aes(x=age+thalach,y=chol))+
  geom_point(aes(y=chol),colour ="Black",cex=0.15)+
  geom_smooth(formula = y~x,method = "lm",colour="Red",alpha=0.1,size=0.35)+
  theme(axis.title.x = element_text(angle = 60,hjust = 2))+
  facet_wrap(~sex,ncol=6,nrow = 5)

##ggplot thalassemia
ggplot(heartdisease,aes(x=age+thalach,y=chol))+
  geom_point(aes(y=chol),colour ="Black",cex=0.15)+
  geom_smooth(formula = y~x,method = "lm",colour="Red",alpha=0.1,size=0.35)+
  theme(axis.title.x = element_text(angle = 60,hjust = 2))+
  facet_wrap(~thal,ncol=6,nrow = 5)

##ggplot thalassemia chest pain type.1: typical angina, 2: atypical angina, 3: non-anginal pain, 4: asymptomatic
ggplot(heartdisease,aes(x=age+thalach,y=chol))+
  geom_point(aes(y=chol),colour ="Black",cex=0.15)+
  geom_smooth(formula = y~x,method = "lm",colour="Red",alpha=0.1,size=0.35)+
  theme(axis.title.x = element_text(angle = 60,hjust = 2))+
  facet_wrap(~cp,ncol=6,nrow = 5)

##ggplot of blooding sugar over >120
ggplot(heartdisease,aes(x=age+thalach,y=chol))+
  geom_point(aes(y=chol),colour ="Black",cex=0.15)+
  geom_smooth(formula = y~x,method = "lm",colour="Red",alpha=0.1,size=0.35)+
  theme(axis.title.x = element_text(angle = 60,hjust = 2))+
  facet_wrap(~fbs,ncol=6,nrow = 5)

 