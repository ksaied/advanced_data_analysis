##################################################################################################
# Two-sample t-test
##################################################################################################
################# Normal-normal distributions #################

set.seed(7)
n = 100000
m = 5
p_values = rep(NA, times=n)

for (i in 1:n){
  sample1 = rnorm(m, mean=0, sd=1)
  sample2 = rnorm(m, mean=0, sd=1)
  p_values[i] = t.test(sample1, sample2, var.equal=TRUE)$p.value
}

sum(p_values < 0.05) /length(p_values)

################# Normal-exponential distributions #################

set.seed(7)
n = 50000
m = 500
p_values = rep(NA, times=n)

for (i in 1:n){
  sample1 = rnorm(m, mean=1, sd=1)
  sample2 = rexp(m, rate = 1)
  p_values[i] = t.test(sample1, sample2, var.equal=TRUE)$p.value
}

sum(p_values < 0.05) /length(p_values)

################# poisson-poisson distributions #################

set.seed(7)
n = 50000
m = 500
p_values = rep(NA, times=n)

for (i in 1:n){
  sample2 = rpois(m)
  sample2 = rpoiss(m)
  p_values[i] = t.test(sample1, sample2, var.equal=TRUE)$p.value
}

sum(p_values < 0.05) /length(p_values)

################# Sample size #################
#10, 100, 500, 1000, 2000, 3000, 5000
#Ideal model but low sample
set.seed(7)
n = 50000
m = 5
p_values = rep(NA, times=n)

for (i in 1:n){
  sample1 = rnorm(m, mean=0, sd=1)
  sample2 = rnorm(m, mean=0, sd=1)
  p_values[i] = t.test(sample1, sample2, var.equal=TRUE)$p.value
}

sum(p_values < 0.05) /length(p_values)

################# Different variances #################
#low sample and unequal variance between the sample -> non-ideal model
set.seed(7)
n = 50000 
m = 5
p_values = rep(NA, times=n)

for (i in 1:n){
  sample1 = rnorm(m, mean=0, sd=50)
  sample2 = rnorm(m, mean=0, sd=3)
  p_values[i] = t.test(sample1, sample2, var.equal=TRUE)$p.value
}

sum(p_values < 0.05) /length(p_values)
#8.2% of p-values < 0.05 with m = 5 and sd1 = 50, sd2 = 3
#--> If we go away from the ideal model (normality and same variance), m must be very big in order to get the expected value of 5%.

##################################################################################################
# Pearson’s correlation
##################################################################################################

#








