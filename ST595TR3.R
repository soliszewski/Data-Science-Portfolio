library(tidyverse)

yrbss_2003 <- readRDS("yrbss_2003.rds")
yrbss_2013 <- readRDS("yrbss_2013.rds")

#Explore the data
head(yrbss_2003)
head(yrbss_2013)

#Reduce the data
yrbss_2003 <- select(yrbss_2003, year, bmi, sex, q33, q81)
yrbss_2013 <- select(yrbss_2013, year, bmi, sex, q33, q81)

#Sample reduced data
head(yrbss_2003)
head(yrbss_2013)

#View BMI by year
ggplot(yrbss_2003, aes(bmi)) +
  geom_histogram(binwidth = 1) +
  labs(title="High-Schooler BMIs in 2003")

ggplot(yrbss_2013, aes(bmi)) +
  geom_histogram(binwidth = 1) +
  labs(title="High-Schooler BMIs in 2013")

#Check for NAs in BMI
sum(is.na(yrbss_2003$bmi))
sum(is.na(yrbss_2013$bmi))

#Determine proportion of NA
sum(is.na(yrbss_2003$bmi))/sum(yrbss_2003$bmi)
sum(is.na(yrbss_2013$bmi))/sum(yrbss_2003$bmi)

#View q33 by year
ggplot(yrbss_2003, aes(q33, fill=sex)) +
  geom_bar(position="dodge") +
  labs(title="High-School Days Smoking in 2003 by Sex")

ggplot(yrbss_2013, aes(q33, fill=sex)) +
  geom_bar(position="dodge") +
  labs(title="High-School Days Smoking in 2013 by Sex")

#Check for NAs in q33
sum(is.na(yrbss_2003$q33))
sum(is.na(yrbss_2013$q33))

#Determine proportion of NA
sum(is.na(yrbss_2003$q33))/sum(yrbss_2003$bmi) * 100
sum(is.na(yrbss_2013$q33))/sum(yrbss_2013$bmi) * 100

#View q81 by year
ggplot(yrbss_2003, aes(q81)) +
  geom_bar() +
  labs(title="High-School TV Watching in 2003")

ggplot(yrbss_2013, aes(q81)) +
  geom_bar() +
  labs(title="High-School TV Watching in 2013")

#Check for NAs in q81
sum(is.na(yrbss_2003$q81))
sum(is.na(yrbss_2013$q81))

#Determine proportion of NA
sum(is.na(yrbss_2003$q81))/sum(yrbss_2003$bmi) * 100
sum(is.na(yrbss_2013$q81))/sum(yrbss_2013$bmi) * 100

#Determine medians
q33.2003.med <- median(as.numeric(yrbss_2003$q33), na.rm = TRUE)
q33.2013.med <- median(as.numeric(yrbss_2013$q33), na.rm = TRUE)
q81.2003.med <- median(as.numeric(yrbss_2003$q81), na.rm = TRUE)
q81.2013.med <- median(as.numeric(yrbss_2013$q81), na.rm = TRUE)

yrbss_2003$q33 <- mutate(q33 =  )

yrbss_2003$q33[is.na(yrbss_2003$q33)] <- levels(yrbss_2003$q33)[q33.2003.med]
yrbss_2013$q33[is.na(yrbss_2013$q33)] <- levels(yrbss_2013$q33)[q33.2013.med]
yrbss_2003$q81[is.na(yrbss_2003$q81)] <- levels(yrbss_2003$q81)[q81.2003.med]
yrbss_2013$q81[is.na(yrbss_2013$q81)] <- levels(yrbss_2013$q81)[q81.2013.med]

#==============================================================
# How has the BMI of high-school students changed between 2003 
# and 2013? Are high-schoolers getting more overweight?
#==============================================================

#Isolate the BMI data
bmi_2003 <- yrbss_2003$bmi
bmi_2013 <- yrbss_2013$bmi

#Run a two sample t-test with alternative hypothesis mu > 0, 
#unpaired and unequal variances
getting_more_overweight <- t.test(bmi_2013, bmi_2003, mu = 0, 
                                  alternative = "greater", paired = FALSE, 
                                  var.equal = FALSE)

#Determine whether to reject the null hypothesis that 2013 has a larger BMI
getting_more_overweight
getting_more_overweight$p.value <= 0.05 

#Determine medians
median(bmi_2003)
median(bmi_2013)

#Create boxplot of BMIs from 2003 and 2013
all_data <- rbind(yrbss_2003,yrbss_2013)
ggplot(data=all_data, aes(year, bmi, group=year)) +
  geom_boxplot(position="dodge") + 
  labs(x="Year", y="BMI", title="High-Schooler BMI in 2003 vs. 2013 with Mean") +
  scale_x_continuous(breaks = seq(2003, 2013, 10),
                   labels = c("2003", "2013")) +
  geom_point(mapping=aes(2003,23.41)) +
  geom_point(mapping=aes(2013,23.64))

#==========================================================
# Are male high-schoolers more likely to smoke than female 
# high-schoolers in 2013?                                  
#==========================================================

#Determine how many students of each sex did not respond with 
#"0 days" in terms of how many times they smoked
#NOTE: "0 days" corresponds to factor value 2
male_smokers_2013 <- sum(as.numeric(yrbss_2013$q33) > 2 & 
                         as.character(yrbss_2013$sex) == "Male", 
                         na.rm = TRUE)
female_smokers_2013 <- sum(as.numeric(yrbss_2013$q33) > 2 & 
                           as.character(yrbss_2013$sex) == "Female", 
                           na.rm = TRUE)

#Determine the total number of students for each sex
total_male_students_2013 <- sum(as.character(yrbss_2013$sex) == "Male")
total_female_students_2013 <- sum(as.character(yrbss_2013$sex) == "Female")    

#Run a two sample test for equal proportions
#NOTE: Proportions are defined as (# Male Smokers / Total # Male Students) and 
#(# Female Smokers / Total # Female Students)
male_more_likely_smoke <- prop.test(
  x = c(male_smokers_2013, female_smokers_2013), 
  n = c(total_male_students_2013, total_female_students_2013), 
  conf.level = 0.95, 
  correct = FALSE, 
  alternative = "greater"
)

#Determine whether to reject null hypothesis that proportions are equal
male_more_likely_smoke
male_more_likely_smoke$p.value <= 0.05

#Count occurences for write-up table
male_smokers_2013
total_male_students_2013
female_smokers_2013
total_female_students_2013

smoking.m <- c("Male", round(male_smokers_2013/total_male_students_2013, 2), "Smoked")
non.smoking.m <- c("Male", round((total_male_students_2013 - male_smokers_2013)/total_male_students_2013, 2), "Did Not Smoke")
smoking.f <- c("Female", round(female_smokers_2013/total_female_students_2013, 2), "Smoked")
non.smoking.f <- c("Female", round((total_female_students_2013 - female_smokers_2013)/total_female_students_2013, 2), "Did Not Smoke")
all.smoking <- rbind(smoking.m,smoking.f, non.smoking.m, non.smoking.f)
colnames(all.smoking) <- c("Sex", "Proportion", "Type")
rownames(all.smoking) <- c()
all.smoking <- data.frame(all.smoking)

ggplot(all.smoking, aes(Type, Proportion, fill=Sex)) +
  geom_col(position="dodge") +
  labs(title="High-School Smoking in 2013 by Sex", x="Q33", y="Proportion") +
  guides(fill=guide_legend(title="Sex"))

#========== Question 2c ===============
# How much TV do high schoolers watch? 
#======================================

#Determine numerical value of median
median_TV_time_numeric <- median(as.numeric(yrbss_2013$q81), na.rm = TRUE)

#Determine the category associated with the numerical median value
median_TV_time_category <- as.character(yrbss_2013$q81)[median_TV_time_numeric]
median_TV_time_category

#Count occurrences for each category for write-up table
TV_time <- na.omit(as.numeric(yrbss_2013$q81))
sum(TV_time == 1)
sum(TV_time == 2)
sum(TV_time == 3)
sum(TV_time == 4)
sum(TV_time == 5)
sum(TV_time == 6)
sum(TV_time == 7)

yrbss_2013 %>% group_by(q81) %>% summarize(Count = sum(!is.na(q81)))

ggplot(yrbss_2013, aes(q81)) +
  geom_bar() +
  labs(title="High-School TV Watching in 2013", x="Q81", y="Count")

