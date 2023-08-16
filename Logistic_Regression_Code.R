#Importing Dataset
library(readxl)
logistic <- read_excel("C:/Users/dhruv/OneDrive/Desktop/Project Nikhil Sir/Prevalence of Diabetes in PLHIV/logistic_data/Practice_logistic_dataset.xlsx")
View(logistic)
summary(logistic)

#Importing Libraries
library(epiDisplay)
library(lmtest)
library(MASS)
library(car)
library(sigmoid)
library(multcomp)
library(ResourceSelection)
library(boot)
library(ggplot2)
library(ggfortify)
library(vcd)



#2x2 contingency table
logistic$Y=ordered(logistic$Y,levels = c(0,1),labels = c("No", "Yes"))
logistic$Smoking=ordered(logistic$Smoking,levels = c(0,1),labels = c("No", "Yes"))
logistic$Snoring=ordered(logistic$Snoring,levels = c(0,1),labels = c("No", "Yes"))
logistic$Obesity=ordered(logistic$Obesity,levels = c(0,1),labels = c("No", "Yes"))

df <- data.frame(logistic)
df2 <- xtabs(Freq ~ Y + Smoking, data = df)
df2

df3 <- xtabs(Freq ~ Y + Snoring, data = df)
df3

df4 <- xtabs(Freq ~ Y + Obesity, data = df)
df4


#Converting Frequency data to Raw data
Smoking <- rep(df$Smoking,times = df$Freq)
Obesity <- rep(df$Obesity, times = df$Freq)
Snoring <- rep(df$Snoring, times = df$Freq)
Y <- rep(df$Y, times = df$Freq)

raw_data <- data.frame(Smoking,Obesity,Snoring,Y)
View(raw_data)


#Simple Logistic Regression for each X
model1 <- glm(Y ~ Smoking, data = raw_data, family = "binomial")#Smoking
summary(model1)
logistic.display(model1)
exp(coef(model1))


model2 <- glm(Y ~ Obesity, data = raw_data, family = "binomial")#Obesity
summary(model2)
logistic.display(model2)
exp(coef(model2))


model3 <- glm(Y ~ Snoring, data = raw_data, family = "binomial")#Snoring
summary(model3)
logistic.display(model3)
exp(coef(model3))


#Fit logistic regression model using all variables
model4 <- glm(Y ~ Smoking + Obesity + Snoring, data = raw_data, family = "binomial")
summary(model4)
#odds ratio will be different as interaction is present


#BOOTSTRAPPED REGRESSION
 
#define function to calculate fitted regression coefficients
coef_function = function(formula, data, indices) {
  d = data[indices,] #allows boot to select sample
  fit = glm(formula, data = d,family = "binomial") #fit regression model
  return(coef(fit)) #return coefficient estimates of model
}

reps <- boot(data=raw_data, statistic=coef_function, R=2000,
             formula=Y ~Smoking+Obesity+Snoring)
reps

# Bootstrap Confidence interval for intercept
CI_1 = boot.ci(reps, type="all", index=1) #intercept of model
CI_1



#Likelihood Ratio Test for model 4
lrtest(model4) #H0 : beta1 = beta2 = beta3
AIC(model4)
BIC(model4)


#Likelihood Ratio Test - Smoking
lr_smo <- glm(Y~Obesity+Snoring,data=raw_data,family = 'binomial')
lrtest(lr_smo,model4)
AIC(lr_smo)
BIC(lr_smo)


#Likelihood Ratio Test - Obesity
lr_obs <- glm(Y~Smoking+Snoring,data=raw_data,family = 'binomial')
lrtest(lr_obs,model4)
AIC(lr_obs)
BIC(lr_obs)

#Likelihood Ratio Test - Snoring
lr_sno <- glm(Y~as.factor(Smoking)+as.factor(Obesity),data=raw_data,family = 'binomial')
lrtest(lr_sno,model4)
AIC(lr_sno)
BIC(lr_sno)

#Likelihood Ratio Test - Snoring and Obesity
lr_snob <- glm(Y~as.factor(Smoking),data=raw_data,family = 'binomial')
lrtest(lr_snob,model4)
AIC(lr_snob)
BIC(lr_snob)


#Fit logistic regression model using forward selection and LR tests
null<-glm(Y ~ 1, data=raw_data,family = "binomial") # 1 here means the intercept 
full<-glm(Y ~ Smoking + Obesity + Snoring, data=raw_data,
          family = 'binomial')
#Forward Selection Code
stepAIC(null, scope=list(lower=null, upper=full), 
        data=raw_data, direction='forward',na.rm =TRUE)
forward_model<-glm(formula = Y ~ as.factor(Obesity) + 
                   as.factor(Snoring), 
                   family = "binomial", 
                   data = raw_data)
summary(forward_model)
forward_vif<-vif(forward_model)
forward_vif


#Fit logistic regression model using backward selection and LR tests
null<-glm(Y~ 1, data=raw_data,family = "binomial") # 1 here means the intercept 
full<-glm(Y~as.factor(Smoking)+as.factor(Obesity)+as.factor(Snoring),data=raw_data,family = 'binomial')

#Backward Elimination Code
stepAIC(full, data = raw_data, direction='backward',
        scope=formula(full)) 
backward_model<-glm(formula = Y ~ as.factor(Obesity) +
                   as.factor(Snoring),
                   family = "binomial", 
                   data = raw_data)
summary(backward_model)
backward_vif<-vif(backward_model)
backward_vif


#Interaction of categorical variables snoring and obesity
#Model with interaction
model_int<-glm(Y ~ Obesity*Snoring + Smoking,
               data = raw_data, family = "binomial")
summary(model_int)


#Model without interaction
model_woint<- glm(Y ~ as.factor(Snoring) + as.factor(Obesity)
                  + as.factor(Smoking), data = new_data, 
                  family="binomial")
summary(model_woint)

#Testing for significane of interactions
lrtest(model_woint,model_int)
# pvalue > 0.05 -> Accept H0 -> Not much difference between 
# models with and without interaction.



#model of Obesity and Snoring
model_snob = glm(formula = Y ~ Obesity + Snoring, 
                family = "binomial", data = raw_data)
summary(model_snob)
logistic.display(model_snob, simplified = TRUE)


#Part h. Calculation of Probabilities from logistic model
#Model for Smoking, Obesity and Snoring
model<- glm(Y ~ as.numeric(Smoking) + as.numeric(Obesity) + as.numeric(Snoring), data = raw_data, family = "binomial")
summary(model)
logistic.display(model, simplified = TRUE)

#Alt model
modelalt<- glm(Y ~ Smoking + Obesity + Snoring, data = raw_data, family = "binomial")
summary(modelalt)
logistic.display(modelalt, simplified = TRUE)


#Calculate log odds
#Smoking+Snoring+constant = 0
names(coef(modelalt))
x1<-summary(glht(modelalt, linfct = c("(Intercept) + Smoking.L + Snoring.L = 0")));x1

#95%CI of estimate
C <-confint(x1);C

#calculate odds
odds = exp(coef(x1));odds
#95% conf intervals of odds
lo=exp(coef(x1) - qnorm(0.975)*x1$test$sigma);lo
hi=exp(coef(x1) + qnorm(0.975)*x1$test$sigma);hi

#calculate probablity
prob = odds/(1+odds);prob
#95% conf intervals of probability
clo <- lo/(1+lo);clo
chi<- hi/(1+hi);chi


#Get predicted probabilities for all observations in the dataset
L<- data.frame(Smoking = c("No","No","No","No","Yes","Yes","Yes","Yes"),
  Obesity =  c("No","No","Yes","Yes","No","No","Yes","Yes"),
  Snoring =  c("Yes","No","Yes","No","Yes","No","Yes","No"));L

xb = predict(modelalt,L,se.fit=TRUE);xb
xbfit = names(table(xb$fit));xbfit

oddsrto = names(table(exp(xb$fit)));oddsrto
L$odds = oddsrto

pro = exp(xb$fit)/(1+exp(xb$fit))
probtab = names(table(pro));probtab
L$prob = probtab

low=exp(xb$fit - qnorm(0.975)*xb$se.fit)
high=exp(xb$fit + qnorm(0.975)*xb$se.fit)
L$ci95_low = names(table(low/(1+low)))
L$ci95_high = names(table(high/(1+high)))
View(L)



#GOODNESS OF FIT TESTS

#Likelihood Ratio Testing
lrtest(model)

# Fit is good as p value < 0.05 -> Reject H0 
# where H0 : Null model provides an equally good fit as 
# the model being compared

#Using chisquare test
A = anova(modelalt,test= "Chisq");A

exp_freq = as.numeric(probtab)*433
obs_freq = data.frame(187,60,51,8,85,17,23,2)
chisq.test(obs_freq, exp_freq)
# p-value > 0.05 -> accept H0 -> model fit is good as there
# is no significant difference between observed and 
# expected frequencies



#Hosmer-Lemeshow goodness-of-fit test
T = hoslem.test(model$y, fitted(model), g = 10);T
#model fits well












