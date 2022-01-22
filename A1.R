#=========================================================================
# A1
#=========================================================================
setwd("C:/Users/÷ÏÃÏ≈‡/613/HW1/Data")
install.packages("tidyverse")
library(tidyverse)
library(gmodels)
library(ineq)
library(dplyr)
library(ggplot2)
#=========================================================================
# Exercise 1:  Basic Statistics
#=========================================================================

#a #Number of households surveyed in 2007
a <- read.csv("dathh2007.csv")
length(a[,2])

#b #Number of households with marital status °∞Couple with kids°± in 2005
b <- read.csv("dathh2005.csv")
b_cwk <- b %>% filter(mstatus == "Couple, with Kids")
nrow(b_cwk)

#c #Number of individuals surveyed in 2008
c <- read.csv("datind2008.csv")
nrow(c)

#d #Number of individuals aged between 25 and 35 in 2016
d <- read.csv("datind2016.csv")
d_age <- d %>% filter(age>=25 & age<=35)
nrow(d_age)

#e #Cross-table gender/profession in 2009
e <- read.csv("datind2009.csv")
print(e)
CrossTable = table(e$gender,e$profession)
CrossTable

#f #Distribution of wages in 2005 and 2019
f2005 <- read.csv("datind2005.csv")
f2019 <- read.csv("datind2019.csv")
#mean, standard deviation
mean(f2005$wage, na.rm = TRUE)
sd(f2005$wage, na.rm = TRUE)
mean(f2019$wage, na.rm = TRUE)
sd(f2019$wage, na.rm = TRUE)
#inter-decile ratio
D9_2005 = quantile(f2005$wage, probs=0.9, na.rm = TRUE)
D1_2005 = quantile(f2005$wage, probs=0.1, na.rm = TRUE)
ratio_2005 = D9_2005/D1_2005
D9_2019 = quantile(f2019$wage, probs=0.9, na.rm = TRUE)
D1_2019 = quantile(f2019$wage, probs=0.1, na.rm = TRUE)
ratio_2019 = D9_2019/D1_2019
#Gini coefficient
ineq(f2005$wage,type="Gini")
ineq(f2019$wage,type="Gini")

#g #Distribution of age in 2010
g <- read.csv("datind2010.csv")
hist(g$age)
male2010 <- g %>% filter(gender == "Male")
female2010 <- g %>% filter(gender == "Female")
hist(male2010$age)
hist(female2010$age)

#h #Number of individuals in Paris in 2011
h_dathh <- read.csv("dathh2011.csv")
h_datind <- read.csv("datind2011.csv")
m_2011 <- merge(h_dathh,h_datind, by.x = "idmen", by.y = "idmen")
Paris <- m_2011 %>% filter(location == "Paris")
nrow(Paris)

#=========================================================================
# Exercise 2:  Merge Datasets
#=========================================================================
#a,b
#Read all individual datasets from 2004 to 2019. Append all these datasets.
#Read all household datasets from 2004 to 2019. Append all these datasets.
get_dathh <- function(year) {
  data_dathh <- read.csv(paste0('dathh', year, '.csv'), header = TRUE)
  return(data.frame(data_dathh))
}

get_datind <- function(year) {
  data_datind <- read.csv(paste0('datind', year, '.csv'), header = TRUE)
  return(data.frame(data_datind))
}

# read 2004 dataset
dathh <- get_dathh(2004)
datind <- get_datind(2004)


for (year in 2005:2019){ 
  # Read datasets
  data_dathh = get_dathh(year)
  data_datind = get_datind(year)
 
  # bind
  dathh <- rbind(dathh, data_dathh)
  datind <- rbind(datind, data_datind)
}

#c #List the variables that are simultaneously present in the individual and household datasets
a = ls(dathh) 
b = ls(datind)
intersect(a,b)

#d #Merge the appended individual and household datasets
mergedata <- merge(dathh,datind,by = c("idmen","year"))

#e # Number of households in which there are more than four family members
member=mergedata %>% group_by(idmen,year) %>%tally()
fouroyed)
mem=member[member$n>4,]
nrow(distinct(fourmem,idmen, .keep_all = TRUE))

#f #Number of households in which at least one member is unemployed
unemployed <- mergedata[mergedata$empstat == "Unemployed",]
unemployed <- unique(subset(unemployed,select=c("year","idmen")))
length(unemployed$idmen)
rm(unemployed)

#g #Number of households in which at least two members are of the same profession
# drop profession = NA or ""
mergedata_with_pro <- mergedata %>% filter(!is.na(profession) & profession != "")
# select idmen, idind, profession
data_selected <- mergedata_with_pro %>% select(idmen, idind, profession)
# unique idmen and idind
dup_idx <- duplicated(data_selected)
data_unique <- data_selected[!dup_idx,]

# count
num_household <- data_unique %>% 
  select(idmen, profession) %>% 
  group_by(idmen, profession) %>% 
  summarise(count=n(), .groups = "drop_last") %>% 
  filter(count >= 2) %>% 
  select(idmen) %>% 
  unique() %>% 
  nrow()
  num_household
  
#h #Number of individuals in the panel that are from household-Couple with kids 
hhcwk <- mergedata[mergedata$mstatus == "Couple, with Kids", ]
count(hhcwk)
rm(hhcwk)  

#i #Number of individuals in the panel that are from Paris
i_paris <- mergedata %>% filter(c(location == "Paris"))
nrow(i_paris)

#k #Number of households present in 2010 and 2011.
n2010and2011 <- mergedata[mergedata$year == 2011 | mergedata$year == 2010, ]
n2010and2011idmen <- subset(n2010and2011, select=c(year, idmen))
n2010and2011idmen <- unique(n2010and2011idmen)
count(n2010and2011idmen)

#=========================================================================
# Exercise 3: Migration
#=========================================================================
#a #Find out the year each household enters and exit the panel
#Report the distribution of the time spent
time <- dathh %>% group_by(idmen) %>% summarize(max(year)-min(year)+1)
qplot(time$`max(year) - min(year) + 1`,geom = 'bar',ylab = 'count',
      xlab = 'duration')

#b #Based on datent, identify whether or not a household moved into its current dwelling at the year of
#survey. Report the first 10 rows of your result and plot the share of individuals in that situation across years.
moveindata <- mergedata %>% mutate(movein = year - datent)
indshare1 <- moveindata %>% group_by(year) %>% summarize(move=length(which(movein == 0)),obs=n(),share1=move/obs)
ggplot(indshare1, aes(x = year,y = share1))+geom_line()+geom_point()

#c #Based on myear and move, identify whether or not household migrated at the year of survey. Report
#the first 10 rows of your result and plot the share of individuals in that situation across years.

migdata <- mergedata %>% mutate(mig = year - myear)
indshare2 <- migdata %>% group_by(year) %>% summarize(migind=length(which(mig == 0)),obs=n(),share2=migind/obs)
ggplot(indshare2, aes(x = year,y = share2))+geom_line()+geom_point()

#d #Mix the two plots you created above in one graph
indsharec <- cbind(indshare1, indshare2)
indsharec1 <- indsharec[,!duplicated(t(indsharec))]
ggplot(indsharec1)+geom_line(aes(x = year,y = share1),color = "red")+
  geom_line(aes(x = year,y = share2),color = "yellow")+ylab("share")

#e #find out how many households had at least one family member changed
#his/her profession or employment status.
mergedata %>% drop_na(datent)
mergedata = mergedata %>% mutate(lagprof = lag(profession,1,order_by=year),lagemp=lag(empstat,1,order_by=year))
mergedata = mergedata %>% mutate(change = ifelse(lagprof != profession | lagemp != empstat,1,0))
change2 = mergedata[which(mergedata["change"]==1),]
length(unique(change2$idind))