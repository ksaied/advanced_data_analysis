#The file birthweigth_study.csv contains data from a study on birth weight, the goal being 
#to identify factors linked to a lower birth weight. The variables registered are:
  
#age   Age of the mother
#lwt   Mother's weight before pregnancy
#smoke Smoking habits
#ht    Known hypertension
#bwt   Newborn weight
#ptd   Preterm birth
#Analyse these data in order to identify variables affecting the birth weight and their 
#possible interactions.

#Using your results, you should produce two reports:
#A one-page report summarizing your results, with the minimal amount of technical details. 
#The target audience of this report is a medical doctor interested in the results of this study.
#A longer report detailing the context in which your data fits ("know your data"), the question 
#that was asked, the analysis of the data, your results and all necessary supporting information. 
#The target audience is another data analyst who has to evaluate your work (e.g. a reviewer).
#=======================================================================================

birthweight <- read.table("birtweight_study.csv", header=TRUE)

birthweight$lwt <- birthweight$lwt*0.453592
birthweight$bwt <- birthweight$bwt/1000

bwt <- birthweight$bwt
lwt <- birthweight$lwt
age <- birthweight$age
ptd <- birthweight$ptd
smoke <- birthweight$smoke
ht <- birthweight$ht

#=======================================================================================
################Pre-analysis################
#=======================================================================================

par(mfrow=c(2,3))
plot(bwt~lwt+smoke+ptd+ht+age, ylab="birth weight [kg]")

#=======================================================================================
################Linear regression################
#=======================================================================================

#Test for interactions
model1 <- lm(bwt~lwt*age+smoke+ht+ptd)
summary(model1)
plot(model1)
#No interaction

model2 <- lm(bwt~lwt*smoke+age+ht+ptd)
summary(model2)
plot(model2)
#No interaction

model3 <- lm(bwt~lwt*ht+smoke+age+ptd)
summary(model3)
plot(model3)
#No interaction

model4 <- lm(bwt~lwt*ptd+ht+smoke+age)
summary(model4)
plot(model4)
#No interaction

model5 <- lm(bwt~age*smoke+lwt+ht+ptd)
summary(model5)
plot(model5)
#Interaction
#The age is never significant, thus this interaction is not taken into account.

model6 <- lm(bwt~age*ht+smoke+lwt+ptd)
summary(model6)
plot(model6)
#Interaction, everything except the age is significant
#The age is never significant, thus this interaction is not taken into account.

model7 <- lm(bwt~age*ptd+ht+smoke+lwt)
summary(model7)
plot(model7)
#No Interaction

model8 <- lm(bwt~ht*ptd+smoke+lwt+age)
summary(model8)
plot(model8)
#No interaction

model9 <- lm(bwt~ht*smoke+ptd+lwt+age)
summary(model9)
plot(model9)
#No interaction

model10 <- lm(bwt~smoke*ptd+ht+lwt+age)
summary(model10)
plot(model10)
#No interaction

#The age is never significant, thus it will be removed from the final analysis.
#Since there is no interaction, the final model will be lm(bwt~lwt+smoke+ptd+ht)

pre_model <- lm(bwt~lwt+smoke+ptd+ht+age)
summary(pre_model)
#The age does not influence the bwt

model <- lm(bwt~lwt+smoke+ptd+ht)
par(mfrow=c(2,2))
plot(model)
summary(model)