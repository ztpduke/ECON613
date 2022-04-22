#=========================================================================
# A4
#=========================================================================
##### Exercise 1####
library(AER)
library(ggplot2)
library(texreg)
data<-read.csv(file.choose(),header=TRUE)
data("dat_A4")
attach(dat_A4)
names(dat_A4)

library(ggplot2)
attach(dat_A4)

age<-2022-KEY_BDATE_Y_1997
work_exp<-CV_WKSWK_JOB_DLI.01_2019/52.14
work_exp

education<- CV_HGC_BIO_DAD_1997+ CV_HGC_BIO_MOM_1997+ CV_HGC_RES_DAD_1997+ CV_HGC_RES_MOM_1997
library(tidyverse)

library("ggplot2")


pp = ggplot(data=dat_A4,aes(x=as.numeric(as.character(X)),y=log(YINC_1700_2019))) + geom_point() + xlab("age") + ylab("age")
pp


pp1 = ggplot(data=dat_A4,aes(x=as.numeric(as.character(KEY_SEX_1997)),y=log(YINC_1700_2019))) + geom_point() + xlab("YINC_1700_2019") + ylab("KEY_SEX_1997")
pp1

pp2 = ggplot(data=dat_A4,aes(x=as.numeric(as.character(CV_BIO_CHILD_HH_U18_2019)),y=log(YINC_1700_2019))) + geom_point() + xlab("YINC_1700_2019") + ylab("CV_BIO_CHILD_HH_U18_2019")
pp2

pdf("Time.pdf")
pp
dev.off()

pdf("State.pdf")


#### Exercise 2#####
suumary(reg)
install.packages("sampleSelection")
library(sampleSelection)
require(maxLik)
require(miscTools)
library(nnet)
library(ggplot2)
library(reshape2)
reg<-lm(YINC_1700_2019~age+ education+KEY_SEX_1997, data=dat_A4)


###### Exercise 3#####

hist(YINC_1700_2019)
vm1 <- lm(log(CV_HGC_BIO_DAD_1997) ~ KEY_SEX_1997, data = dat_A4)
coeftest(vm1, vcov = sandwich)

summary(vm1)


data("dat_A4", package="plm")
dat = dat_A4
library(stargazer)
stargazer(dat)

##### Exercise 4 ######


spec1 <- lm(log(CV_WKSWK_JOB_DLI.01_2019)~log(age)+log(education)+log(KEY_SEX_1997),data=dat_A4)
spec2 <- lm(CV_WKSWK_JOB_DLI.01_2019~age+education+KEY_SEX_1997,data=dat_A4)

summary(spec2)
summary(spec1)


texreg::texreg(list(spec1,spec2))
