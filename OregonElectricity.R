#Sam Oliszewski
#March 23, 2018
#Oregon Electricity

elec <- read.csv("or_acs_house.csv")
elec <- elec[c("NP","BLD","ACR","BDSP","FULP","GASP","HFL","RMSP","TEN","VALP","YBL","R18","R60","ELEP")]

#--- Perform Median/Mode Imputation on Missing Data ---

findACRMode <- function(all_ACR) {
  factor1 <- sum(all_ACR == 1, na.rm=TRUE)
  factor2 <- sum(all_ACR == 2, na.rm=TRUE)
  factor3 <- sum(all_ACR == 3, na.rm=TRUE)
  allFactors <- c(factor1, factor2, factor3)
  which.max(allFactors)
}

findBLDMode <- function(all_BLD) {
  factor1 <- sum(all_BLD == 1, na.rm=TRUE)
  factor2 <- sum(all_BLD == 2, na.rm=TRUE)
  factor3 <- sum(all_BLD == 3, na.rm=TRUE)
  factor4 <- sum(all_BLD == 4, na.rm=TRUE)
  factor5 <- sum(all_BLD == 5, na.rm=TRUE)
  factor6 <- sum(all_BLD == 6, na.rm=TRUE)
  factor7 <- sum(all_BLD == 7, na.rm=TRUE)
  factor8 <- sum(all_BLD == 8, na.rm=TRUE)
  factor9 <- sum(all_BLD == 9, na.rm=TRUE)
  factor10 <- sum(all_BLD == 10, na.rm=TRUE)
  allFactors <- c(factor1, factor2, factor3, factor4, 
                  factor5, factor6, factor7, factor8, 
                  factor9, factor10)
  which.max(allFactors)
}

findHFLMode <- function(all_HFL) {
  factor1 <- sum(all_HFL == 1, na.rm=TRUE)
  factor2 <- sum(all_HFL == 2, na.rm=TRUE)
  factor3 <- sum(all_HFL == 3, na.rm=TRUE)
  factor4 <- sum(all_HFL == 4, na.rm=TRUE)
  factor5 <- sum(all_HFL == 5, na.rm=TRUE)
  factor6 <- sum(all_HFL == 6, na.rm=TRUE)
  factor7 <- sum(all_HFL == 7, na.rm=TRUE)
  factor8 <- sum(all_HFL == 8, na.rm=TRUE)
  factor9 <- sum(all_HFL == 9, na.rm=TRUE)
  allFactors <- c(factor1, factor2, factor3, factor4, 
                  factor5, factor6, factor7, factor8, 
                  factor9)
  which.max(allFactors)
}

findTENMode <- function(all_TEN) {
  factor1 <- sum(all_TEN == 1, na.rm=TRUE)
  factor2 <- sum(all_TEN == 2, na.rm=TRUE)
  factor3 <- sum(all_TEN == 3, na.rm=TRUE)
  factor4 <- sum(all_TEN == 4, na.rm=TRUE)
  allFactors <- c(factor1, factor2, factor3, factor4)
  which.max(allFactors)
}

findYBLMode <- function(all_YBL) {
  factor1 <- sum(all_YBL == 1, na.rm=TRUE)
  factor2 <- sum(all_YBL == 2, na.rm=TRUE)
  factor3 <- sum(all_YBL == 3, na.rm=TRUE)
  factor4 <- sum(all_YBL == 4, na.rm=TRUE)
  factor5 <- sum(all_YBL == 5, na.rm=TRUE)
  factor6 <- sum(all_YBL == 6, na.rm=TRUE)
  factor7 <- sum(all_YBL == 7, na.rm=TRUE)
  factor8 <- sum(all_YBL == 8, na.rm=TRUE)
  factor9 <- sum(all_YBL == 9, na.rm=TRUE)
  factor10 <- sum(all_YBL == 10, na.rm=TRUE)
  factor11 <- sum(all_YBL == 11, na.rm=TRUE)
  factor12 <- sum(all_YBL == 12, na.rm=TRUE)
  factor13 <- sum(all_YBL == 13, na.rm=TRUE)
  factor14 <- sum(all_YBL == 14, na.rm=TRUE)
  factor15 <- sum(all_YBL == 15, na.rm=TRUE)
  factor16 <- sum(all_YBL == 16, na.rm=TRUE)
  factor17 <- sum(all_YBL == 17, na.rm=TRUE)
  factor18 <- sum(all_YBL == 18, na.rm=TRUE)
  factor19 <- sum(all_YBL == 19, na.rm=TRUE)
  allFactors <- c(factor1, factor2, factor3, factor4, 
                  factor5, factor6, factor7, factor8, 
                  factor9, factor10, factor11, factor12, 
                  factor13, factor14, factor15, factor16,
                  factor17, factor18, factor19)
  which.max(allFactors)
}

findR18Mode <- function(all_R18) {
  factor1 <- sum(all_R18 == 1, na.rm=TRUE)
  factor2 <- sum(all_R18 == 2, na.rm=TRUE)
  allFactors <- c(factor1, factor2)
  which.max(allFactors)
}

findR60Mode <- function(all_R60) {
  factor1 <- sum(all_R60 == 1, na.rm=TRUE)
  factor2 <- sum(all_R60 == 2, na.rm=TRUE)
  allFactors <- c(factor1, factor2)
  which.max(allFactors)
}

all_NP <- elec$NP
all_NP[is.na(all_NP)] <- median(all_NP, na.rm=TRUE)
elec$NP <- all_NP

all_ACR <- as.integer(elec$ACR)
all_ACR[is.na(all_ACR)] <- findACRMode(all_ACR)
all_ACR <- factor(all_ACR, labels=c(
  "House on less than one acre",
  "House on one to less than ten acres",
  "House on ten or more acres"
  ))
elec$ACR <- all_ACR

all_BDSP <- as.integer(elec$BDSP)
all_BDSP[is.na(all_BDSP)] <- median(all_BDSP, na.rm=TRUE)
elec$BDSP <- all_BDSP

all_BLD <- as.integer(elec$BLD)
all_BLD[is.na(all_BLD)] <- findBLDMode(all_BLD)
all_BLD <- factor(all_BLD, labels=c(
  "Mobile home or trailer",
  "One-family house detached",
  "One-family house attached",
  "2 Apartments",
  "3-4 Apartments",
  "5-9 Apartments",
  "10-19 Apartments",
  "20-49 Apartments",
  "50 or more apartments",
  "Boat, RV, van, etc."
  ))
elec$BLD <- all_BLD

all_FULP <- as.integer(elec$FULP)
all_FULP[is.na(all_FULP)] <- median(all_FULP, na.rm=TRUE)
elec$FULP <- all_FULP

all_GASP <- as.integer(elec$GASP)
all_GASP[is.na(all_GASP)] <- median(all_GASP, na.rm=TRUE)
elec$GASP <- all_GASP

all_HFL <- as.integer(elec$HFL)
all_HFL[is.na(all_HFL)] <- findHFLMode(all_HFL)
all_HFL <- factor(all_HFL, labels=c(
  "Utility gas",
  "Bottled, tank, or LP gas",
  "Electricity",
  "Fuel oil, kerosene, etc.",
  "Coal or coke",
  "Wood",
  "Solar energy",
  "Other fuel",
  "No fuel used"
  ))
elec$HFL <- all_HFL

all_RMSP <- as.integer(elec$RMSP)
all_RMSP[is.na(all_RMSP)] <- median(all_RMSP, na.rm=TRUE)
elec$RMSP <- all_RMSP

all_TEN <- as.integer(elec$TEN)
all_TEN[is.na(all_TEN)] <- findTENMode(all_TEN)
all_TEN <- factor(all_TEN, labels=c(
  "Owned with mortgage or loan (include home equity loans)",
  "Owned free and clear",
  "Rented",
  "Occupied without payment of rent"
  ))
elec$TEN <- all_TEN

all_VALP <- as.integer(elec$VALP)
all_VALP[is.na(all_VALP)] <- median(all_VALP, na.rm=TRUE)
elec$VALP <- all_VALP

all_YBL <- as.integer(elec$YBL)
all_YBL[is.na(all_YBL)] <- findYBLMode(all_YBL)
all_YBL <- factor(all_YBL, labels=c(
  "1939 or earlier",
  "1940 to 1949",
  "1950 to 1959",
  "1960 to 1969",
  "1970 to 1979",
  "1980 to 1989",
  "1990 to 1999",
  "2000 to 2004",
  "2005",
  "2006",
  "2007",
  "2008",
  "2009",
  "2010",
  "2011",
  "2012",
  "2013",
  "2014",
  "2015"
  ))
elec$YBL <- all_YBL

all_R18 <- as.integer(elec$R18)
all_R18[is.na(all_R18)] <- findR18Mode(all_R18)
all_R18 <- factor(all_R18, labels=c(
  "No person under 18 in household",
  "1 or more persons under 18 in household"
  ))
elec$R18 <- all_R18

all_R60 <- as.integer(elec$R60)
all_R60[is.na(all_R60)] <- findR60Mode(all_R60)
all_R60 <- factor(all_R60, labels=c(
  "No person 60 and over",
  "1 or more persons 60 and over in household"
))
elec$R60 <- all_R60

all_ELEP <- as.integer(elec$ELEP)
all_ELEP[is.na(all_ELEP)] <- median(all_ELEP, na.rm=TRUE)
elec$ELEP <- all_ELEP

#End Median/Mode Imputation 

elec_full <- elec

#Select observations from elec where BLD is 
#house or apartment

elec <- subset(elec, as.integer(BLD) != 1 & as.integer(BLD) != 10)
elec_house <- subset(elec, as.integer(BLD) == 2 | as.integer(BLD) == 3)
elec_house$BLD <- as.factor("House")
elec_apartment <- subset(elec, as.integer(BLD) != 2 & as.integer(BLD) != 3)
elec_apartment$BLD <- as.factor("Apartment")

elec <- rbind(elec_house, elec_apartment)

#--- Exploratory Analysis ---
library(ggplot2)

#Plots of all relevant predictors against the response
plot(elec$NP, elec$ELEP, xlab="Number of People", ylab="Price of Electricity", main="Price of Electricity by Number of People")
plot(elec$ACR, elec$ELEP, xlab="Lot Size", ylab="Price of Electricity", main="Price of Electricity by Lot Size")
plot(elec$BDSP, elec$ELEP, xlab="Number of Bedrooms", ylab="Price of Electricity", main="Price of Electricity by Number of Bedrooms")
plot(elec$BLD, elec$ELEP, xlab="Units in Structure", ylab="Price of Electricity", main="Price of Electricity by Units in Structure")
plot(elec$FULP, elec$ELEP, xlab="Fuel Cost", ylab="Price of Electricity", main="Price of Electricity by Fuel Cost")
plot(elec$GASP, elec$ELEP, xlab="Gas Cost", ylab="Price of Electricity", main="Price of Electricity by Gas Cost")
plot(elec$HFL, elec$ELEP, xlab="House Heating Fuel Type", ylab="Price of Electricity", main="Price of Electricity by House Heating Fuel Type")
plot(elec$RMSP, elec$ELEP, xlab="Number of Rooms", ylab="Price of Electricity", main="Price of Electricity by Number of Rooms")
plot(elec$TEN, elec$ELEP, xlab="Tenure", ylab="Price of Electricity", main="Price of Electricity by Tenure")
plot(elec$VALP, elec$ELEP, xlab="Property Value", ylab="Price of Electricity", main="Price of Electricity by Property Value")
plot(elec$YBL, elec$ELEP, xlab="Year Structure Built", ylab="Price of Electricity", main="Price of Electricity by Year Structure was Built")
plot(elec$R18, elec$ELEP, xlab="Presence of Under Age 18 Persons", ylab="Price of Electricity", main="Price of Electricity by Presence of Under Age 18 Persons")
plot(elec$R60, elec$ELEP, xlab="Presence of Over Age 60 Persons", ylab="Price of Electricity", main="Price of Electricity by Presence of Over Age 60 Persons")
plot(elec_full$BLD, elec_full$ELEP, xlab="Units in Structure", ylab="Price of Electricity", main="Price of Electricity by Units in Structure")

#Residual Analysis
library(broom)
library(ggplot2)

mod <- lm(ELEP ~ ., data = elec)
mod_diag <- broom::augment(mod, data = elec)

#Fitted vs. Residuals
qplot(.fitted, .resid, data = mod_diag)

#Predictors vs. Residuals
qplot(NP, .resid, data = mod_diag)
qplot(ACR, .resid, data = mod_diag)
qplot(BDSP, .resid, data = mod_diag)
qplot(BLD, .resid, data = mod_diag)
qplot(FULP, .resid, data = mod_diag)
qplot(GASP, .resid, data = mod_diag)
qplot(HFL, .resid, data = mod_diag)
qplot(RMSP, .resid, data = mod_diag)
qplot(TEN, .resid, data = mod_diag)
qplot(VALP, .resid, data = mod_diag)
qplot(YBL, .resid, data = mod_diag)
qplot(R18, .resid, data = mod_diag)
qplot(R60, .resid, data = mod_diag)

#--- Explanatory Problem Section ---
library(broom)
library(ggplot2)

mod <- lm(ELEP ~ ., data = elec)
mod_diag <- broom::augment(mod, data = elec)
qplot(.fitted, .resid, data = mod_diag)
mod_int <- lm(ELEP ~ .*., data=elec)
mod_int_diag <- broom::augment(mod_int, data = elec)
qplot(.fitted, .resid, data = mod_int_diag)

anova(mod, mod_int)

summary(mod_int)
confint(mod_int)

#--- Prediction Problem Section ---
library(ISLR)
library(leaps)
set.seed(100)

#Validation Set Approach
train <- sample(c(TRUE,FALSE), nrow(elec_full), rep=TRUE)
test <- (!train)

regfit.best <- regsubsets(ELEP ~ ., data=elec_full[train,],
                          nvmax=13)

test.mat <- model.matrix(ELEP ~ ., data=elec_full[test,])

val.errors <- rep(NA, 13)
for(i in 1:13){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[, names(coefi)]%*%coefi
  val.errors[i] <- mean((elec_full$ELEP[test]-pred)^2)
}

val.errors
which.min(val.errors)

predict.regsubsets <- function(object, newdata, id ,...){
  form=as.formula (object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#k-Fold Cross Validation
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(elec_full), replace=TRUE)
cv.errors <- matrix(NA, k, 13, dimnames=list(NULL, paste(1:13)))
for(j in 1:k){
  best.fit <- regsubsets(ELEP ~ ., data=elec_full[folds!=j,], nvmax=13)
  for(i in 1:13){
    pred <- predict(best.fit, elec_full[folds!=j,], id=i)
    cv.errors[j, i] <- mean((elec_full$ELEP[folds!=j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
which.min(mean.cv.errors)

summary(lm(ELEP ~ ., data=elec_full))
