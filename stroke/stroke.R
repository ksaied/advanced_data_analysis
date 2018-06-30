install.packages("ISwR")
library(ISwR)
library(survival)
data(stroke)

summary(stroke)
?stroke

#Number of observations:
nrow(stroke)
ncol(stroke)
#Bias in the dataset
#Censored data 

hist(stroke$age)
####################################################################################################
#Visualisation
####################################################################################################

plot(stroke$age~stroke$sex, col=c("red", "blue"))
t.test(stroke$age[stroke$sex=="Female"], stroke$age[stroke$sex=="Male"])
#Males are more likely to have stroke when they are younger than females.

#Building the survival object:
stroke_surv = Surv(stroke$obsmonths, stroke$dead=="TRUE") #Plus-signs identify those observations that are right-censored

#Kaplan-Meier estimate:
stroke_surv_fit = survfit(stroke_surv ~ 1 )
stroke_surv_fit
#It tells us that among the 829 records, 485 patients were followed-up until they died (uncensored).
#Among these 485 events (deaths), there is a median survival time of 20 months (median because of censored data???)
summary(stroke_surv_fit)

#The Kaplan-Meier estimate is plotted -> survival curve (Kaplan-Meier plot)
plot(stroke_surv_fit, main="Survival for stroke patients", xlab="Time after diagnosis (months)", ylab="Fraction of survivors", conf.int=T, mark.time=T)
#It gives the proportion of patients that have survived for each time point.
#"+" signs indicate each time a person left the study.

#Assessment of the association between sex and survival time:
#Kaplan-Meier estimate by sex:
stroke_surv_bysex = survfit(stroke_surv~stroke$sex) #by sex
stroke_surv_bysex
summary(stroke_surv_bysex)

#Kaplan-Meier curves according to sex
plot(stroke_surv_bysex, xlab="Months after diagnosis", ylab="Proportion of survivors", main="Survival over time by sex", col=c("red", "blue"),conf.int=T,mark.time=T)
legend("topright",legend = c("Female","Male"),fill = c("red","blue"))

#Is there a statistical significant difference between survival curves? -> log-rank test.
#Null hypothesis: there is no significant difference in the survival time between males and females that have suffered from stroke.
survdiff(stroke_surv~stroke$sex)
#The null hypothesis is rejected. There is a significant difference in survival time between males and females (Chisq= 14.6, p-value=0.000132)

#Is there a significant difference in the survival time between males and females after adjusting for age?
#-> Cox Proportional Hazards model

cox_sex_age = coxph(stroke_surv~stroke$sex+stroke$age)
summary(cox_sex_age)
#Males have 1.02258 times the risk of dying than females but it's not significant.
#We realize that, regarding the survdiff, what's significant, it not really the sex but the age. Because the cox test show that sex is no more significant.

cox_all = coxph(stroke_surv~stroke$sex+stroke$age+stroke$dgn+stroke$coma+stroke$diab+stroke$minf+stroke$han)
summary(cox_all)

cox_dgn = coxph(stroke_surv~stroke$dgn) 
cox_dgn

cox_coma = coxph(stroke_surv~stroke$coma) #significant
summary(cox_coma)

cox_diab = coxph(stroke_surv~stroke$diab) #Not significant
summary(cox_diab)

cox_minf = coxph(stroke_surv~stroke$minf) #significant
summary(cox_minf)

cox_han = coxph(stroke_surv~stroke$han) #Not significant
summary(cox_han)
