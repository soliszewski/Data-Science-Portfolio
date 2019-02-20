# Sam Oliszewski
# February 1, 2018
# New Zealand Crashes

library(VGAM)
library(tidyverse)
library(MASS)

# Read in each of the datasets and convert to format with three columns: Hour, Day, Crashes
crash.t <- crashtr %>% mutate(Hour = row_number()-1) %>% 
          gather(-Hour, key="Day",value="Crashes") %>%
          mutate(Time = cut(Hour,
                            breaks = c(-1, 5.5, 11.5, 18.5, 25),
                            labels = c("Early.Morning", "Morning", "Afternoon", "Evening")), 
                 Day = as.character(Day)) %>%
          group_by(Day, Time) %>% 
          summarize(Crashes = sum(Crashes))

crash.t$Day <- factor(crash.t$Day, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

crash.m <- crashmc %>% mutate(Hour = row_number()-1) %>% 
          gather(-Hour, key="Day",value="Crashes") %>%
          mutate(Time = cut(Hour,
                            breaks = c(-1, 5.5, 11.5, 18.5, 25),
                            labels = c("Early.Morning", "Morning", "Afternoon", "Evening")), 
                 Day = as.character(Day)) %>%
          group_by(Day, Time) %>% 
          summarize(Crashes = sum(Crashes))

crash.m$Day <- factor(crash.m$Day, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

crash.b <- crashbc %>% mutate(Hour = row_number()-1) %>% 
          gather(-Hour, key="Day",value="Crashes") %>%
          mutate(Time = cut(Hour,
                            breaks = c(-1, 5.5, 11.5, 18.5, 25),
                            labels = c("Early.Morning", "Morning", "Afternoon", "Evening")), 
                 Day = as.character(Day)) %>%
          group_by(Day, Time) %>% 
          summarize(Crashes = sum(Crashes))

crash.b$Day <- factor(crash.b$Day, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

# Determine number of zero counts in data
filter(crash.t, Crashes == 0)
filter(crash.m, Crashes == 0)
filter(crash.b, Crashes == 0) # Only one zero count in all three datasets, no evidence of zero-inflation 

# Explore relationship between time of day, day of week, and number of crashes
ggplot(crash.t, aes(Day, Crashes, fill=Time)) +
  geom_col(position="dodge") + 
  labs(title="Truck Crashes by Day and Time")

ggplot(crash.m, aes(Day, Crashes, fill=Time)) +
  geom_col(position="dodge") +
  labs(title="Motorcycle Crashes by Day and Time")

ggplot(crash.b, aes(Day, Crashes, fill=Time)) +
  geom_col(position="dodge") +
  labs(title="Bicycle Crashes by Day and Time")

# Fit a log-linear model to determine dispersion parameter
crash.t.mod.ll <- glm(Crashes ~ Day + Time, family=poisson, data=crash.t) 
crash.m.mod.ll <- glm(Crashes ~ Day + Time, family=poisson, data=crash.m)
crash.b.mod.ll <- glm(Crashes ~ Day + Time, family=poisson, data=crash.b)
summary(crash.t.mod.ll) # Dispersion parameter (residual deviance): 23.859/18 = 1.33
summary(crash.m.mod.ll) # Dispersion parameter (residual deviance): 53.165/18 = 2.95
summary(crash.b.mod.ll) # Dispersion parameter (residual deviance): 20.242/18 = 1.12

# Check residuals for log-linear models
ggplot(data=crash.t, aes(crash.t.mod.ll$fitted.values, resid(crash.t.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", x="Fitted", title="Residuals versus Fitted - Log-Linear Model for Truck Crashes")
ggplot(data=crash.t, aes(Day, resid(crash.t.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Day - Log-Linear Model for Truck Crashes")
ggplot(data=crash.t, aes(Time, resid(crash.t.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Time - Log-Linear Model for Truck Crashes")

ggplot(data=crash.m, aes(crash.m.mod.ll$fitted.values, resid(crash.m.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", x="Fitted", title="Residuals versus Fitted - Log-Linear Model for Motorcycle Crashes")
ggplot(data=crash.m, aes(Day, resid(crash.m.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Day - Log-Linear Model for Motorcycle Crashes")
ggplot(data=crash.m, aes(Time, resid(crash.m.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Time - Log-Linear Model for Motorcycle Crashes")

ggplot(data=crash.b, aes(crash.b.mod.ll$fitted.values, resid(crash.b.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", x="Fitted", title="Residuals versus Fitted - Log-Linear Model for Bicycle Crashes")
ggplot(data=crash.b, aes(Day, resid(crash.b.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Day - Log-Linear Model for Bicycle Crashes")
ggplot(data=crash.b, aes(Time, resid(crash.b.mod.ll))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Time - Log-Linear Model for Bicycle Crashes")

# Fit negative binomial models to the data to account for evidence of over dispersion
crash.m.mod.nb <- glm.nb(Crashes ~ Day + Time, data=crash.m)
summary(crash.m.mod.nb) 

# Check residuals from fitted model
ggplot(data=crash.m, aes(crash.m.mod.nb$fitted.values, resid(crash.m.mod.nb))) + 
  geom_point() + 
  labs(y="Residuals", x="Fitted", title="Residuals versus Fitted - Negative Binomial Model for Motorcycle Crashes")
ggplot(data=crash.m, aes(Day, resid(crash.m.mod.nb))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Day - Negative Binomial Model for Motorcycle Crashes")
ggplot(data=crash.m, aes(Time, resid(crash.m.mod.nb))) + 
  geom_point() + 
  labs(y="Residuals", title="Residuals versus Time - Negative Binomial Model for Motorcycle Crashes")

# Predict the number of bicycle crashes that we expect to observe on a Wednesday afternoon
Day <- "Wed"
Time <- "Afternoon"
predict.data <- data.frame(Day,Time)

predictions <- predict(crash.b.mod.nb, predict.data, type="response", se.fit=TRUE)

to_predict <- mutate(predict.data, Row = row_number())
(prediction_summary <- data.frame(predictions) %>% 
    group_by(Row = row_number()) %>% 
    summarize(Estimate = round(fit,2), Lower.Bound = round(fit - se.fit,2), Upper.Bound = round(fit + se.fit,2), 
              SE = round(se.fit,2)) %>% left_join(to_predict) %>% 
    as.data.frame())
