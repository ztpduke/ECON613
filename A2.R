#=========================================================================
# A2
#=========================================================================
setwd("C:/Users/ÖìÌìÅà/613/HW2")
library(tidyverse)
install.packages("pacman")
library(dplyr)
install.packages("margins")
library(margins)
install.packages("mfx")
library(mfx)
#=========================================================================
# Exercise 1:  OLS estimate
#=========================================================================
adata <- read.csv("datind2009.csv")
#a #Calculate the correlation between Y and X
cor(adata$age,adata$wage, use = "complete.obs")
# the correlation is -0.1788512

#b #Calculate the coefficients on this regression
bdata <- adata %>% mutate(intercept = 1) 
bdata.reg <- data.frame(bdata$wage,bdata$age,bdata$intercept)
bdata.reg.c <- na.omit(bdata.reg)
y <- as.matrix(bdata.reg.c[,c(1)])
x <- as.matrix(bdata.reg.c[,c(3,2)])
A =  solve(t(x)%*%x)
beta = A%*%t(x)%*%y
table(beta)
#beta
#-180.176457223401  22075.1065764526
lm(wage~age, data = bdata)
#c #Calculate the standard errors of ¦Â
#Using the standard formulas of the OLS
resi <- y-x%*%beta
N <- nrow(x)
K <- ncol(x)
print(K)
resi.var = t(resi)%*%resi/(N-K)
beta.var <- resi.var[1,1] * A
beta.se <- sqrt(diag(beta.var))
print(beta.se)
#bdata.intercept       bdata.age 
#v357.827521         6.968652

#Using bootstrap with 49 and 499 replications respectively
p = 49
q = 499

#49 replications
#storage 
intercept_DT <- rep(0,p)
slope_DT <- rep(0,p)

for (i in 1:p){
  
  bs.49 <- bdata.reg.c[sample(N,N,replace = TRUE),]
  bs.49_x <- as.matrix(bs.49[,c(2,3)])
  bs.49_y <- as.matrix(bs.49[,c(1)])
  bs.49_beta <- solve(t(bs.49_x)%*%bs.49_x)%*%t(bs.49_x)%*%bs.49_y
  intercept_DT[i] <- bs.49_beta[2]
  slope_DT[i] <- bs.49_beta[1]
                              
}

bs.49_se_intercept <- sd(intercept_DT)
bs.49_se_slope <- sd(slope_DT)
print(bs.49_se_intercept)
print(bs.49_se_slope)
# 293.1   4.766283

#499 replications
#storage 
intercept_DT <- rep(0,q)
slope_DT <- rep(0,q)

for (i in 1:q){
  
  bs.499 <- bdata.reg.c[sample(N,N,replace = TRUE),]
  bs.499_x <- as.matrix(bs.499[,c(2,3)])
  bs.499_y <- as.matrix(bs.499[,c(1)])
  bs.499_beta <- solve(t(bs.499_x)%*%bs.499_x)%*%t(bs.499_x)%*%bs.499_y
  intercept_DT[i] <- bs.499_beta[2]
  slope_DT[i] <- bs.499_beta[1]
  
}

bs.499_se_intercept <- sd(intercept_DT)
bs.499_se_slope <- sd(slope_DT)
print(bs.499_se_intercept)
print(bs.499_se_slope)
#314.265 5.598984
#=========================================================================
# Exercise 2:  Detrend Data
#=========================================================================

#a #Create a categorical variable ag
# read all documents

get_datind <- function(year) {
  data_datind <- read.csv(paste0('datind', year, '.csv'), header = TRUE)
  return(data.frame(data_datind))
}
# read 2005 dataset
datind <- get_datind(2005)


for (year in 2006:2018){ 
  # Read datasets
  data_datind = get_datind(year)
  # bind
  datind <- rbind(datind, data_datind)
}

datind$ag <- case_when(datind$age >= 18 & datind$age <=25~"18-25",
                       datind$age >= 26 & datind$age <=30~"26-30",
                       datind$age >= 31 & datind$age <=35~"31-35",
                       datind$age >= 36 & datind$age <=40~"36-40",
                       datind$age >= 41 & datind$age <=45~"41-45",
                       datind$age >= 46 & datind$age <=50~"46-50",
                       datind$age >= 51 & datind$age <=55~"51-55",
                       datind$age >= 56 & datind$age <=60~"56-60",
                     60<= datind$age ~"60+")

#b #Plot the wage of each age group across years
ggplot(data = datind, mapping = aes(x= year, y= wage, color = factor(ag))) + 
  geom_boxplot()  

#c #Consider Yit = ¦ÂXit + ¦Ãt + eit. After including a time fixed effect, how do the estimated coefficients
#change?

FEM <- lm(wage~age + factor(year), data = datind)
summary(FEM)
#=========================================================================
# Exercise 3:  Detrend Data
#=========================================================================
#a #Exclude all individuals who are inactive
datind2007 <- read.csv("datind2007.csv")
n = which(datind2007$empstat == "Inactive")
datindexc_Detrend <-datind2007[-n,]

#b #Write a function that returns the likelihood of the probit of being employed
datindexc_Detrend$empvalue <- case_when(datindexc_Detrend$empstat =="Unemployed"~0,
                                        datindexc_Detrend$empstat =="Retired"~0,
                                        datindexc_Detrend$empstat =="Employed"~1)

probit_1 = glm(empvalue ~ age, family = binomial(link = "probit"), data = datindexc_Detrend)
summary(probit_1)

logLik(probit_1)
#'log Lik.' -6582.155 (df=2)

#c #Optimize the model and interpret the coefficients.
optim(par, probit, gr = NULL, ...,
      method = c("L-BFGS-B"),
      lower = -Inf, upper = Inf,
      control = list(), hessian = FALSE)
#=========================================================================
# Exercise 4:  Discrete choice
#=========================================================================
#a  #Exclude all individuals who are inactive
n = which(datind$empstat == "Inactive")
datindexc_discrete <-datind[-n,]

#b #Write and optimize the probit, logit, and the linear probability models
datindexc_discrete$empvalue <- case_when(datindexc_discrete$empstat =="Unemployed"~0,
                                         datindexc_discrete$empstat =="Retired"~0,
                                         datindexc_discrete$empstat =="Employed"~1)

probit_2 = glm(empvalue ~ age, family = binomial(link = "probit"), data = datindexc_discrete)
summary(probit_2)
logit_2 = glm(empvalue ~ age, family = binomial(link = "logit"), data = datindexc_discrete)
summary(logit_2)
lpm_2 = lm(empvalue ~ age, data = datindexc_discrete)
summary(lpm_2)
#=========================================================================
# Exercise 5:  Marginal Effects
#=========================================================================
#a #Compute the marginal effect of the previous probit and logit models
probit_1margins <- margins(probit_1)
probit_2margins <- margins(probit_2)
logit_2margins <- margins(logit_2)

#b #Construct the standard errors of the marginal effects
probitmfx(empvalue ~ age , data=datindexc_Detrend, atmean = FALSE)
probitmfx(empvalue ~ age , data=datindexc_discrete, atmean = FALSE)
logitmfx(empvalue ~ age , data=datindexc_discrete, atmean = FALSE)
