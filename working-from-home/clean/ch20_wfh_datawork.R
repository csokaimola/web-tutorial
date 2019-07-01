


# Chapter 20 - work-from-home using Bloom et al. (2015): Does Working from Home Work?  Evidence from a Chineses Experiment. QJE. 165-218



library(tidyverse)
library(haven)

dir <-  "C:/Users/banki/Dropbox/bekes_kezdi_textbook/"

# Set location folders -----------------------------------------------
data_in <- paste0(dir,"cases_studies_public/working-from-home/raw/")
data_out <- paste0(dir,"cases_studies_public/working-from-home/clean/")

# to compile final data set, we use 3 data sources
data_quit <- read_dta(paste0(data_in, "quit_data.dta"))
data_tc <- read_dta(paste0(data_in, "tc_comparison.dta"))
data_perf <- read_dta(paste0(data_in, "performance_during_exper.dta"))

# first combine 1st and 2nd source
data_join <- inner_join(data_quit, data_tc, by = c("personid"))

# make some changes to joint df
data_join <- data_join %>% select(-perform10_expgroup, -perform11_expgroup, -matches("\\.y")) %>% 
  rename(male = men.x, age = age.x, costofcommute = costofcommute.x, children = children.x, treatment = expgroup.x, married = married.x)



# prepare 3rd source: data_perf; keep if expgroup is 0 or 1
data_perf <- data_perf %>% filter(expgroup == 0 | expgroup ==1) %>% mutate(experiment_time = experiment_treatment + experiment_control)

data_perf <- data_perf %>% mutate(phonecalls = phonecallraw / 1000) %>% select(personid, year_week, experiment_time, treatment, phonecalls)


# write_dta(data_perf, paste0(data_out, "wfh_tidy_personweek.dta"))


# aggregates from 3rd source that will be used as new columns in final combined df
data_sum <- data_perf %>% group_by(personid, experiment_time) %>% summarise(sum = sum(phonecalls, na.rm = TRUE))

data_sum <- data_sum %>% spread(key = experiment_time, value = sum, sep = "") %>% 
  rename(phonecalls0 = experiment_time0, phonecalls1 = experiment_time1)

# merge aggregates from 3rd df
data_tidy <- inner_join(data_join, data_sum, by = c("personid"))

# add new variable
data_tidy <- mutate(data_tidy, ordertaker = type == 1)

# reorder columns
nm1 <- c("personid", "treatment", "ordertaker", "type", "quitjob")

data_tidy <- data_tidy[,c(nm1,setdiff(names(data_tidy),nm1))]

# write_dta(data_perf, paste0(data_out, "wfh_tidy_person.dta")




