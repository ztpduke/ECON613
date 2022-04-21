library(ggplot2)
attach(dat_A4)
age<- KEY_BDATE_M_1997-2022
age<-2022-KEY_BDATE_Y_1997
work_exp<-CV_WKSWK_JOB_DLI.01_2019/52.14
work_exp

education<- CV_HGC_BIO_DAD_1997+ CV_HGC_BIO_MOM_1997+ CV_HGC_RES_DAD_1997+ CV_HGC_RES_MOM_1997
library(tidyverse)
ggplot2::aes(y=YINC_1700_2019$age)

ggplot(data = dat_A4,
       mapping = aes(x =KEY_SEX_1997 , y = YINC_1700_2019))+ geom_point()

ggplot(data = dat_A4,
       mapping = aes(x = YINC_1700_2019, y = KEY_SEX_1997)) +
  geom_point()

ggplot(dat_A4, aes(x =KEY_SEX_1997 , y = YINC_1700_2019)) + 
  geom_point()






ggplot(dat_A4, aes(date)) + 
  geom_line(aes(y = YINC_1700_2019, colour = "blue")) + 
  geom_line(aes(y = age, colour = "red"))

library("reshape2")
library("ggplot2")
test_data_long <- melt(dat_A4, id="x")  # convert to long format

age

ggplot(data=dat_A4,
       aes(x=YINC_1700_2019, y=age)) +
  geom_line()


reg<-lm(YINC_1700_2019~age+ education+KEY_SEX_1997, data=dat_A4)
suumary(reg)
summary(reg)
install.packages("sampleSelection")
library(sampleSelection)
require(maxLik)
require(miscTools)
library(nnet)
library(ggplot2)
library(reshape2)

reg1<-heckit(YINC_1700_2019~age+ education+KEY_SEX_1997, data=dat_A4)
heckman=heckit(YINC_1700_2019~age+ education+KEY_SEX_1997,
               log(YINC_1700_2019) ~ education + age, data=dat_A4)

heck1 = heckit( lfp ~ age + I( age^2 ) + kids + huswage + educ,
                log(wage) ~ educ + exper + I( exper^2 ) + city, data=Mroz87 )

str(age)
hist(YINC_1700_2019$KEY_SEX_1997)
str(KEY_SEX_1997)

dat_A4$age

hist(YINC_1700_2019$age)
is.atomic(dat_A4)
is.recursive(dat_A4)


hist(YINC_1700_2019)
install.packages("survminer")
library(survival)
library(survminer)
library(dplyr)

surv_object <- Surv(time = YINC_1700_2019$age, event = YINC_1700_2019$age)

fit1 <- survfit(YINC_1700_2019~age+KEY_SEX_1997, data=dat_A4)

