library(ggplot2)
set.seed(1234)

yrbss_2003 <- readRDS("yrbss_2003.rds")
yrbss_2013 <- readRDS("yrbss_2013.rds")

#---Simulation Study Section---

#Using variable names for readability of code for question 1
bmi_2003 <- yrbss_2003$bmi
bmi_2013 <- yrbss_2013$bmi

#========== Question 1a ==========

#*** This function will be used for subsequent parts of question 1.

#Draw samples of n size from the bmi data from 2013. 
draw_sample <- function(n) {
  sample <- sample(x = bmi_2013, size = n)
}

#***

#Take 10,000 samples of size 10, 100, and 1,000 from the bmi_2013 data set and determine the sample mean
size_10_sample_mean <- replicate(10000, mean(draw_sample(10)))
size_100_sample_mean <- replicate(10000, mean(draw_sample(100)))
size_1000_sample_mean <- replicate(10000, mean(draw_sample(1000)))

#Create the x-axis values
x_sample_mean <- seq(from = 20, to = 28, length = 1000)

#Create the y-axis values and normal distribution overlay for n = 10 samples
y_size_10_sample_mean <- dnorm(x_sample_mean, mean = mean(size_10_sample_mean), sd = sqrt(mean(bmi_2013)/10))
qplot(size_10_sample_mean, binwidth=0.5, xlim=c(20,28)) + 
  geom_line(aes(y = 10000*0.5*y_size_10_sample_mean, x = x_sample_mean), 
            size = 1.5, color = "blue") + 
  labs(x = "Sample Mean", y = "Count", 
       title = "Sampling Distribution of the Sample Mean of Size 10 Samples")

#Create the y-axis values and normal distribution overlay for n = 100 samples
y_size_100_sample_mean <- dnorm(x_sample_mean, 
                                mean = mean(size_100_sample_mean), 
                                sd = sqrt(mean(bmi_2013)/100))
qplot(size_100_sample_mean, binwidth=0.1, xlim=c(20,28)) + 
  geom_line(aes(y = 10000*0.1*y_size_100_sample_mean, x = x_sample_mean), 
            size = 1.5, color = "blue") +
  labs(x = "Sample Mean", y = "Count", 
       title = "Sampling Distribution of the Sample Mean of Size 100 Samples")

#Create the y-axis values and normal distribution overlay for n = 1,000 samples
y_size_1000_sample_mean <- dnorm(x_sample_mean, 
                                 mean = mean(size_1000_sample_mean), 
                                 sd = sqrt(mean(bmi_2013)/1000))
qplot(size_1000_sample_mean, binwidth=0.1, xlim=c(20,28)) + 
  geom_line(aes(y = 10000*0.1*y_size_1000_sample_mean, x = x_sample_mean), 
            size = 1.5, color = "blue") +
  labs(x = "Sample Mean", y = "Count", 
       title = "Sampling Distribution of the Sample Mean of Size 1,000 Samples")

#Find the mean of each sampling distribution for n = 10, 100 and 1,000
mean(size_10_sample_mean)
mean(size_100_sample_mean)
mean(size_1000_sample_mean)

#Find the standard deviation of each sampling distribution for n = 10, 
#100 and 1,000
sd(size_10_sample_mean)
sd(size_100_sample_mean)
sd(size_1000_sample_mean)

#Find the true population mean
mean(bmi_2013)

#========== Question 1b ==========

#Using the draw_sample function defined in question 1a to draw the samples 
#and find the 25th percentile

#Take 10,000 samples of size 10, 100, and 1,000 from the bmi_2013 data set 
#and determine the 25th percentile
size_10_quantile <- replicate(10000, quantile(draw_sample(10), 0.25))
size_100_quantile <- replicate(10000, quantile(draw_sample(100), 0.25))
size_1000_quantile <- replicate(10000, quantile(draw_sample(1000), 0.25))

#Create the x-axis values
x_quantile <- seq(from = 17, to = 25, length = 1000)

#Create the y-axis values and normal distribution overlay for n = 10 samples
y_size_10_quantile <- dnorm(x_quantile, mean = mean(size_10_quantile), 
                            sd = sqrt(mean(size_10_quantile)/10))
qplot(size_10_quantile, binwidth=0.5, xlim=c(17,25)) + 
  geom_line(aes(y = 10000*0.5*y_size_10_quantile, x = x_quantile), 
            size = 1.5, color = "blue") +
  labs(x = "Sample 25th Percentile", y = "Count", 
       title = "Sampling Distribution of the Sample 25th Percentile of Size 10 Samples")

#Create the y-axis values and normal distribution overlay for n = 100 samples
y_size_100_quantile <- dnorm(x_quantile, mean = mean(size_100_quantile), 
                             sd = sqrt(mean(size_100_quantile)/100))
qplot(size_100_quantile, binwidth=0.1, xlim=c(17,25)) + 
  geom_line(aes(y = 10000*0.1*y_size_100_quantile, x = x_quantile), 
            size = 1.5, color = "blue") +
  labs(x = "Sample 25th Percentile", y = "Count", 
       title = "Sampling Distribution of the Sample 25th Percentile of Size 100 Samples")

#Create the y-axis values and normal distribution overlay for n = 1,000 samples
y_size_1000_quantile <- dnorm(x_quantile, mean = mean(size_1000_quantile), 
                              sd = sqrt(mean(size_1000_quantile)/1000))
qplot(size_1000_quantile, binwidth=0.1, xlim=c(17,25)) + 
  geom_line(aes(y = 10000*0.1*y_size_1000_quantile, x = x_quantile), 
            size = 1.5, color = "blue") +
  labs(x = "Sample 25th Percentile", y = "Count", 
       title = "Sampling Distribution of the Sample 25th Percentile of Size 1,000 Samples")

#Find the mean of each sampling distribution for n = 10, 100 and 1,000
mean(size_10_quantile)
mean(size_100_quantile)
mean(size_1000_quantile)

#Find the standard deviation of each sampling distribution for n = 10, 100 and 1,000
sd(size_10_quantile)
sd(size_100_quantile)
sd(size_1000_quantile)

#Find the true population 25th percentile
quantile(bmi_2013, 0.25)

#========== Question 1c ==========

#Using the draw_sample function defined in question 1a to draw the samples and find the sample minimum

#Take 10,000 samples of size 10, 100, and 1,000 from the bmi_2013 data set and determine the sample minimum
size_10_minimum <- replicate(10000, min(draw_sample(10)))
size_100_minimum <- replicate(10000, min(draw_sample(100)))
size_1000_minimum <- replicate(10000, min(draw_sample(1000)))

#Create the x-axis values
x_minimum <- seq(from = 12, to = 22, length = 1000)

#Create the y-axis values and normal distribution overlay for n = 10 samples
y_size_10_minimum <- dnorm(x_minimum, mean = mean(size_10_minimum), 
                           sd = sqrt(mean(size_10_minimum)/10))
qplot(size_10_minimum, binwidth=0.5, xlim=c(12,22)) + 
  geom_line(aes(y = 10000*0.5*y_size_10_minimum, x = x_minimum), 
            size = 1.5, color = "blue") +
  labs(x = "Sample Minimum", y = "Count", 
       title = "Sampling Distribution of the Sample Minimum of Size 10 Samples")

#Create the y-axis values and normal distribution overlay for n = 100 samples
y_size_100_minimum <- dnorm(x_minimum, mean = mean(size_100_minimum), 
                            sd = sqrt(mean(size_100_minimum)/100))
qplot(size_100_minimum, binwidth=0.1, xlim=c(12,22)) + 
  geom_line(aes(y = 10000*0.1*y_size_100_minimum, x = x_minimum), 
            size = 1.5, color = "blue") +
  labs(x = "Sample Minimum", y = "Count", 
       title = "Sampling Distribution of the Sample Minimum of Size 100 Samples")

#Create the y-axis values and normal distribution overlay for n = 1,000 samples
y_size_1000_minimum <- dnorm(x_minimum, mean = mean(size_1000_minimum), 
                             sd = sqrt(mean(size_1000_minimum)/1000))
qplot(size_1000_minimum, binwidth=0.1, xlim=c(12,22)) + 
  geom_line(aes(y = 10000*0.1*y_size_1000_minimum, x = x_minimum), 
            size = 1.5, color = "blue") +
  labs(x = "Sample Minimum", y = "Count", 
       title = "Sampling Distribution of the Sample Minimum of Size 1,000 Samples")

#Find the mean of each sampling distribution for n = 10, 100 and 1,000
mean(size_10_minimum)
mean(size_100_minimum)
mean(size_1000_minimum)

#Find the mean of each sampling distribution for n = 10, 100 and 1,000
sd(size_10_minimum)
sd(size_100_minimum)
sd(size_1000_minimum)

#Determine the true population minimum
min(bmi_2013)

#========== Question 1d ==========
#This function draws a sample from the BMIs of 2003 and 2013 and 
#calculates the difference in the sample medians
find_diffs_sample_median <- function(n) {
  sample_2003 <- sample(x = bmi_2003, size = n)
  sample_2013 <- sample(x = bmi_2013, size = n)
  median(sample_2013) - median(sample_2003)
}

#Take 10,000 samples of size 10, 100, and 1,000 from the bmi_2013 data set and determine the
#difference in sample medians
diffs_sample_median_5 <- replicate(10000, find_diffs_sample_median(5))
diffs_sample_median_10 <- replicate(10000, find_diffs_sample_median(10))
diffs_sample_median_100 <- replicate(10000, find_diffs_sample_median(100))

#Create the x-axis values
x_diffs <- seq(from = -8, to = 8, length = 1000)

#Create the y-axis values and normal distribution overlay for n = 5 samples
y_size_5_diffs <- dnorm(x_diffs, mean = mean(diffs_sample_median_5), 
                           sd = sqrt(mean(diffs_sample_median_5)/5))
qplot(diffs_sample_median_5, binwidth=0.1, xlim=c(-8,8)) + 
  geom_line(aes(y = 10000*0.1*y_size_5_diffs, x = x_diffs), 
            size = 1.5, color = "blue") +
  labs(x = "Difference in Sample Medians", y = "Count", 
       title = "Sampling Distribution of the Difference in Sample Medians of Size 5 Samples")

#Create the y-axis values and normal distribution overlay for n = 10 samples
y_size_10_diffs <- dnorm(x_diffs, mean = mean(diffs_sample_median_10), 
                        sd = sqrt(mean(diffs_sample_median_10)/10))
qplot(diffs_sample_median_10, binwidth=0.1, xlim=c(-8,8)) + 
  geom_line(aes(y = 10000*0.1*y_size_10_diffs, x = x_diffs), 
            size = 1.5, color = "blue") +
  labs(x = "Difference in Sample Medians", y = "Count", 
       title = "Sampling Distribution of the Difference in Sample Medians of Size 10 Samples")

#Create the y-axis values and normal distribution overlay for n = 100 samples
y_size_100_diffs <- dnorm(x_diffs, mean = mean(diffs_sample_median_100), 
                         sd = sqrt(mean(diffs_sample_median_100)/100))
qplot(diffs_sample_median_100, binwidth=0.1, xlim=c(-8,8)) + 
  geom_line(aes(y = 10000*0.1*y_size_100_diffs, x = x_diffs), 
            size = 1.5, color = "blue") +
  labs(x = "Difference in Sample Medians", y = "Count", 
       title = "Sampling Distribution of the Difference in Sample Medians of Size 100 Samples")

#Find the mean of each sampling distribution for n = 5, 10 and 100
mean(diffs_sample_median_5)
mean(diffs_sample_median_10)
mean(diffs_sample_median_100)

#Find the standard deviation of each sampling distribution for n = 5, 10 and 100
sd(diffs_sample_median_5)
sd(diffs_sample_median_10)
sd(diffs_sample_median_100)

#Find the true difference in population medians
median(bmi_2013) - median(bmi_2003)

#---Data Analysis Section---

#======================= Question 2a ==========================
# How has the BMI of high-school students changed between 2003 
# and 2013? Are high-schoolers getting more overweight?
#==============================================================

#Run a two sample t-test with alternative hypothesis mu > 0, 
#unpaired and unequal variances
getting_more_overweight <- t.test(bmi_2013, bmi_2003, mu = 0, 
                                  alternative = "greater", paired = FALSE, 
                                  var.equal = FALSE)

#Determine whether to reject the null hypothesis that 2013 has a larger BMI
getting_more_overweight
getting_more_overweight$p.value <= 0.05 

#Create boxplot of BMIs from 2003 and 2013
boxplot(bmi_2003, at=1, xlim=c(0, 3), col="#1188d8", medcol="#55e03c", 
        whiskcol="#0a5384", outcol="#7200ff")
boxplot(bmi_2013, at=2, add=TRUE, col="#dd27bf", medcol="#55e03c", 
        whiskcol="#910e7b", outcol="#ffaa00")
points(1:2, c(23.41, 23.64), bg="black", pch=21)
text(1.11:2.11, c(23.41, 23.64), labels=c(23.41, 23.64))
axis(1, at=1:2, labels=c("2003", "2013")) 
axis(2, at = seq(0,60,5))
lapply(seq(0,5,0.1), function(x) abline(a = x,b = 0))
title(main = "Boxplots of BMI Data From 2003 and 2013 with Mean Labeled", 
      xlab="Year", ylab="BMI")

#===================== Question 2b ========================
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



