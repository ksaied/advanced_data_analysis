#The dataset "Aribidopsis" (available in file arabidopsis.csv) was collected to study the 
#fertility of Arabidopsis thaliana according to certain variables. The experiment involved 
#two variables of particular interest: the first one is a fertilization treatment, and the 
#second one involves simulated herbivory (plants were "clipped" to mimic the effect of an 
#herbivore eating part of the plant).

#The detail of the variables is as follows:
#'reg' region: a factor indicating where the plant comes from ('NL' (Netherlands), 'SP' 
#(Spain), or 'SW' (Sweden))
#'popu' population: a factor with the form 'n.R' representing a population in region 'R'
#'gen' genotype: a factor with 24 (numeric-valued) levels
#'rack' a nuisance factor with 2 levels, one for each of two greenhouse racks
#'nutrient' fertilization treatment/nutrient level (1, minimal nutrients or 8, added 
#nutrients)
#'amd' simulated herbivory or "clipping" (apical meristem damage): 'unclipped' (baseline) 
#or 'clipped'
#'status' a nuisance factor for germination method ('Normal', 'Petri.Plate', or 'Transplant')
#'total.fruits' total fruit set per plant (integer) is our measure of fertility (response variable)
#Write a report describing the data, and identifying the factors that influence the 
#fertility.

#Your report should be targeted at a biologist who has an idea about statistics: he/she 
#will understand the models you are going to use and wants to know about them, but does 
#not need all the details (these should be provided in an appendix)

#==========================================================================================
#Importing file
#==========================================================================================

#install.packages("lme4")
#install.packages("nlme")
library(Matrix)
library(lme4)
arabid <- read.csv("arabidopsis.csv", header=TRUE)

#==========================================================================================
#Data description
#==========================================================================================

nutrient <- arabid$nutrient #two-levels factor => fixed
amd <- arabid$amd #two-levels factor => fixed

gen <- arabid$gen #twenty-four-levels factor => random
reg <- arabid$reg #three-levels factor => random
popu <- arabid$popu #nine-levels factor => random
rack <- arabid$rack #two-levels factor => random
status <- arabid$status #three-levels factor => random

total.fruits <- arabid$total.fruits #discrete variable (response variable)

hist(total.fruits, breaks=20, main="Distribution of total fruits number", xlab="Total fruits number")
boxplot(total.fruits~nutrient*amd, col=c("white","lightgray"))
legend("topleft", legend=c("1: minimal nutrient", "8: added nutrient"))

#Transformation of numeric variables into factors
gen <- as.factor(gen)
rack <- as.factor(rack)
nutrient <- as.factor(nutrient)

#Visualization of factors
plot(reg, total.fruits)
plot(popu, total.fruits)
plot(gen, total.fruits)
plot(rack, total.fruits)
plot(nutrient, total.fruits)
plot(amd, total.fruits)
plot(status, total.fruits)

#Visualization of the balance between levels within factors
summary(arabid)
sort(table(arabid[,1])) #reg, not balanced
sort(table(arabid[,2])) #popu, not balanced
sort(table(arabid[,3])) #gen, not balanced
sort(table(arabid[,7])) #status, not balanced

#Nesting
xtabs(~arabid$reg+arabid$popu) #populations are nested within reg
xtabs(~arabid$popu+arabid$gen) #gen is nested within popu

#==========================================================================================
#Analysis
#==========================================================================================

#Discrete response variable -> glm fitting a Poisson distribution.

pre_model = glm(total.fruits~nutrient*amd, family=poisson)
summary(pre_model)$coefficients

model = glmer(total.fruits~nutrient*amd+(1|gen)+(1|popu)+(1|rack)+(1|reg)+(1|status), family=poisson)
summary(model)$coefficients #Fixed effects
summary(model)$varcor #random effects

#When mixed model, fixed effects are far more interesting -> As a first step, glm with fixed effects
#Then, addition of some random variables if we think they're important. 
#Random variables will not change the estimate! They will maybe improve the p-values. Selection of random variables is less important than the fixed effects.
