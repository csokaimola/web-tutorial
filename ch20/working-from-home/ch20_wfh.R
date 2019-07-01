######################################################################
#
# Data Analysis Textbook
# Case Study for Chapter 20 - Experiments
# Data : work-from-home
# Using Bloom et al. (2015): Does Working from Home Work? Evidence from a Chineses Experiment. QJE. 165-218
#
######################################################################
#
# What this code does:
#

######################################################################

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(haven)
library(dplyr)
library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer)

# Change working directory -------------------------------------------
# dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
dir <-  "C:/Users/viktoriakonya/Dropbox/bekes_kezdi_textbook/"

# Set location folders -----------------------------------------------
data_in <- paste0(dir,"cases_studies_public/working-from-home/clean/")
data_out <- paste0(dir,"textbook_work/ch20/working-from-home/")
func <- paste0(dir, "textbook_work/ch00_tech_prep/")
output <- paste0(dir,"textbook_work/ch20/working-from-home/output/")


# Load in data -------------------------------------------------------
data <- read_dta(paste(data_in,"wfh_tidy_person.dta",sep=""))

data <- data %>% select(personid:perform11, age, male, second_technical, high_school, tertiary_technical, university,
                        prior_experience, tenure, married, children, ageyoungestchild, rental,
                        costofcommute, internet, bedroom, basewage, bonus, grosswage)


# Balance ------------------------------------------------------------

# Modify variable
data$ageyoungestchild <- ifelse(data$children == 0, NA, data$ageyoungestchild)


# Table of averages in control and treatment

data_temp <- data %>% 
  select (age:grosswage)


vars <- colnames(data_temp)
rm(data_temp)

mean_t <- c()
mean_c <- c()
sd <- c()
p_value <- c()
model <- c()


for(i in vars){
  # Regression model
  model <- lm(paste(i, "~treatment"), data=data) 
  
  # Mean control
  mean_c[i] <- mean(data[data$treatment==0, ][[paste(i)]], na.rm=T)
  # mean_c[i] <- model$coefficients[1] # or get it directly from regression
  
  # Mean treated
  mean_t[i] <- mean(data[data$treatment==1, ][[paste(i)]], na.rm=T)
  # mean_t[i] <- model$coefficients[1] + model$coefficients[2] # or get it directly from regression
  
  # p-value from regression
  p_value[i] <- anova(model)$'Pr(>F)'[1]
  
  # Standard devition
  sd[i] <- sd(data[[paste(i)]], na.rm=T)
}

# Put together 
table <- data.frame(round(mean_t, 2), round(mean_c, 2), round(sd, 2), round(p_value, 2))

col.names <- c("Treatment mean", "Control mean", "Std.dev.", "p-value of test of equal means")  
names(table) <- col.names
print(table)


# --------------------------------------------------------------------
# Regression analysis 
# Outcome variables: 1) quit firm during 8 months of experiment , 2) phone calls worked, for ordertakers
# --------------------------------------------------------------------

# Outcomes by treatment

# 1) Quit firm
data %>%
  group_by(treatment) %>%
  summarise_at(vars(quitjob),  funs(N=n(), Mean=mean(., na.rm=T), Sd=sd(., na.rm=T)))

# 2) Phonecalls (ordertakers only)
data %>%
  group_by(treatment) %>%
  filter(ordertaker==1) %>%
  summarise_at(vars(phonecalls1),  funs(N=n(), Mean=mean(., na.rm=T), Sd=sd(., na.rm=T)))



# Regression 1: ATE estimates, no covariates -------------------------

reg <- lm(quitjob ~ treatment, data=data)
reg1 <- coeftest(reg, vcov = sandwich)

reg <- lm(phonecalls1 ~ treatment, data=data[data$ordertaker==1, ])
reg2 <- coeftest(reg, vcov = sandwich)

stargazer(reg1, reg2, out=paste(output,"Ch20_wfh_reg1_R.tex",sep=""), digits=2, float = F, no.space = T)


# Regression 2: ATE estimates, with covariates of some unbalance -----
reg <- lm(quitjob ~ treatment + married + children + internet, data=data)
reg3 <- coeftest(reg, vcov = sandwich)

reg <- lm(phonecalls1 ~ treatment + married + children + internet, data=data[data$ordertaker==1, ])
reg4 <- coeftest(reg, vcov = sandwich)


stargazer(reg3, reg4, out=paste(output,"Ch20_wfh_reg2_R.tex",sep=""), digits=2, float = F, no.space = T)





