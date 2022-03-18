#=========================================================================
# A3
#=========================================================================
## ---- include=FALSE-------------------------------------------------------------
knitr::opts_chunk$set(echo = T, warning=FALSE, message=FALSE, attr.source='.numberLines', attr.output='.numberLines', fig.align='center', fig.width=10, fig.height=7)


## -------------------------------------------------------------------------------
library(tidyverse)
datjss <- read.csv("datjss.csv", row.names=1)
# head(datjss)
datsss <- read.csv("datsss.csv", row.names=1)
# head(datsss)
datstu <- read.csv("datstu_v2-1.csv", row.names=1, 
                   na.strings = c(" ", "NA", ""))
# head(datstu)


#=========================================================================
# Exercise 1:  Basic Statistics
#=========================================================================
# Number of students, schools, programs
number_of_students <- nrow(datstu)
number_of_students


## -------------------------------------------------------------------------------
number_of_schools <- datstu %>% 
  select(contains("school")) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = !id, names_to = "Program") %>% 
  distinct(value) %>% count()
number_of_schools$n


## -------------------------------------------------------------------------------
# Number of programs
number_of_programs <- datstu %>% 
  select(contains("pgm")) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = !id, names_to = "Program") %>% 
  distinct(value) %>% 
  na.omit() %>% count()
number_of_programs$n


## -------------------------------------------------------------------------------
programs_only <- datstu %>% 
  select(contains("pgm")) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = !id, names_to = "ProgramNo", 
               values_to = "Program") 

schools_only <- datstu %>% 
  select(contains("school")) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = !id, names_to = "SchoolNo",
               values_to = "School") 
#
number_of_choices <- cbind.data.frame(schools_only, programs_only) %>% 
  select(School, Program) %>% 
  group_by(School, Program) %>% 
  count() %>% 
  pivot_wider(names_from = Program, values_from = n, values_fill = 0)
number_of_choices


## -------------------------------------------------------------------------------
# Number of students applying to at least one senior high schools in 
#the same district to home (Suppose students live in the same district 
# to their junior high schools)
school_jss <- datstu %>% 
  select(contains("school"), jssdistrict) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = schoolcode1:schoolcode6, names_to = "SchoolNo",
               values_to = "schoolcode")
school_jss_datsss <- merge(x = school_jss, datsss, by = 'schoolcode')
#
live_same_senior_junior_apply <- school_jss_datsss %>% 
  select(jssdistrict, sssdistrict) %>% 
  mutate(loc = ifelse( # Partial String Matching
    grepl(sssdistrict, jssdistrict, ignore.case = T),1,0)) %>% 
  summarise(sum = sum(loc))
live_same_senior_junior_apply$sum


## -------------------------------------------------------------------------------
# Delete this chunk if your data has the score and rankplace
# Number of students each senior high school admitted
set.seed(12) 
# Had to fill up data with random values as not provided for score and rankplace
datstu$score <- sample.int(n = 100, size = dim(datstu)[1], replace = T)
datstu$rankplace <- sample.int(n = 2, size = dim(datstu)[1], replace = T)-1


## -------------------------------------------------------------------------------
school_score_rank <- datstu %>% 
  select(contains("school"), rankplace, score) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = schoolcode1:schoolcode6, names_to = "SchoolNo",
               values_to = "School") %>% 
  filter(rankplace==1)
senior_highschool_admitted <- school_score_rank %>% 
  group_by(School) %>% 
  summarise(n = n())
senior_highschool_admitted


## -------------------------------------------------------------------------------
# The cutoff of senior high schools (the lowest score to be admitted)
senior_highschool_cutoff_low <- min(school_score_rank$score)
senior_highschool_cutoff_low


## -------------------------------------------------------------------------------
# The quality of senior high schools (the average score of students admitted)
senior_highschool_cutoff_high <- mean(school_score_rank$score)
senior_highschool_cutoff_high


#=========================================================================
# Exercise 2:  Data
#=========================================================================

# 
programs_score_only <- datstu %>% 
  select(contains("pgm"), score, jssdistrict, rankplace) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = choicepgm1:choicepgm6, names_to = "ProgramNo", 
               values_to = "Program")

schools_only <- datstu %>% 
  select(contains("school")) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = !id, names_to = "SchoolNo",
               values_to = "School") 
#
school_data <- cbind.data.frame(
  schools_only, programs_score_only) 
school_data$id <- NULL 
school_data <- school_data %>% 
  filter(rankplace == 1)
#
Q2_data <- merge(datjss, school_data, by = "jssdistrict")
#
school_cutoff_quality_size <- school_score_rank %>% 
  group_by(School, ) %>% 
  summarise(
    cutoff = min(score),
    quality = mean(score),
    size = n()
  )
#
Q2_data_1 <- merge(x = Q2_data, y = school_cutoff_quality_size, 
                   by = "School")
head(Q2_data_1)


#=========================================================================
# Exercise 3:  Distance
#=========================================================================

#
Q3_data <- merge(x = Q2_data_1, y = datsss, 
                 by.x = "School", by.y = "schoolcode")
Q3_data$dist_sss_jss = sqrt(
  (69.172*(Q3_data$ssslong- Q3_data$point_x)*cos(Q3_data$point_y/57.3))^2 + 
    (69.172*(Q3_data$ssslat = Q3_data$point_y))^2)
head(Q3_data)


#=========================================================================
# Exercise 4:  Dimensionality Reduction
#=========================================================================

Q4_data <- Q3_data


## -------------------------------------------------------------------------------
# Recode the schoolcode into its frst three digits (substr). 
# Call this new variable scode rev.
Q4_data$scode_rev <- str_sub(Q4_data$School, 1,4)


## -------------------------------------------------------------------------------
#
Q4_data$Program <- factor(
  Q4_data$Program, 
  levels = 
    c("Accounting","Agric. Mechanics","Agriculture",
      "Auto Body Works","Block Laying & Concreting","Business",
      "Carpentry & Joinery","Catering","Electrical Installation Works",
      "Electrical Mach. Rew.","Fashion Design","Furniture Craft",
      "General Arts","General Science","Home Economics",
      "Industrial Mechanics","Mech. Eng. Craft Pract.",
      "Motor Vehicle Mech.","Painting & Decorating",
      "Plumbing & Gas Fitting","Printing","Refrigeration & Air Cond.",
      "Small Eng. Repairs","Technical","Visual Arts","Welding & Fabrication"),
  
  labels = 
    c("Others","Others","Others", "Others","Others","economics",
  "Others","Others","Others", "Others","Others","Others","Arts","Science",
  "Economics","Others","Others","Others","Others","Others",
  "Others","Others","Others","Others","Arts","Others"))
Q4_data$pgm_rev <- Q4_data$Program


## -------------------------------------------------------------------------------
# Create a new choice variable choice rev.
Q4_data$choice_rev <- str_sub(Q4_data$ProgramNo, -1)


## -------------------------------------------------------------------------------
# Recalculate the cutoff and the quality for each recoded choice.
recoded_choices_data <- Q4_data %>% 
  group_by(scode_rev, pgm_rev, choice_rev) %>% 
  summarise(
    cutoff = min(score),
    quality = mean(score),
    size = n()
  )
#
head(recoded_choices_data)


## -------------------------------------------------------------------------------
new_data <- merge(
  Q4_data, recoded_choices_data, 
  by = c("scode_rev","pgm_rev","choice_rev"))


#=========================================================================
# Exercise 5:  First Model
#=========================================================================
Q5_data <- Q4_data[complete.cases(Q4_data),]
Q5_data <- distinct(Q5_data)
new_data <- Q5_data
new_data$choice_rev <- factor(
  new_data$choice_rev, 
  levels = c("1","2","3","4","5","6"),
  ordered = T)


## -------------------------------------------------------------------------------
# individual identifier: id
# Decision variable: choice_rev
# choice_rev alternatives: "4" "1" "2" "3" "6" "5"
# Independent variable: score
# For student i, being assigned to choice_rev j
# choice_rev_{ij} = c_j + \gamma score + \epsilon_{ij}
# In order to run the ordinal logistic regression model, 
# we need the polr function from the MASS package
library(MASS)
library(margins)
library(effects)


## -------------------------------------------------------------------------------
# Build ordinal logistic regression model
OLRmodel_5 <- polr(choice_rev ~ score , data = new_data, Hess = T)
#summary(OLRmodel_5)


## -------------------------------------------------------------------------------
# We can use the coef() function to check the parameter estimates
(OLRestimates_5 <- coef(summary(OLRmodel_5)))
# Marginal Effects
summary(margins(OLRmodel_5))


#=========================================================================
# Exercise 6:  Second Model
#=========================================================================
new_data$Program <- as.factor(new_data$Program)

# Build ordinal logistic regression model
OLRmodel_6 <- polr(
  choice_rev ~ quality + dist_sss_jss + cutoff + size + Program, 
  data = new_data, Hess = T)
# summary(OLRmodel_6)
# We can use the coef() function to check the parameter estimates
(OLRestimates_6 <- coef(summary(OLRmodel_6)))
# 
# marginal_effects(OLRmodel_6)
summary(margins(OLRmodel_6))

#=========================================================================
# Exercise 7:  Counterfactual simulations
#=========================================================================
probabilitie_s <- predict(OLRmodel_6, type = "probs")
head(probabilitie_s)


## -------------------------------------------------------------------------------
new_data <- subset(new_data, Program != "Others")
new_data$Program <- as.factor(new_data$Program)

# Build ordinal logistic regression model
OLRmodel_7 <- polr(
  choice_rev ~ quality + dist_sss_jss + cutoff + size + Program, 
  data = new_data, Hess = T)
# summary(OLRmodel_6)
probabilitie_s2 <- predict(OLRmodel_7, type = "probs")
head(probabilitie_s2)

