/*Read in data from CSV file*/
filename data 'Veteran.csv';
data vet;
  infile data firstobs=2 delimiter=',';
  input trt celltype time status karno diagtime age prior;  
run;

/*Define the categorical equivalents for treatment, celltype, and prior for readability*/
proc format;
value treatment
1="Standard Therapy"
2="Test Chemotherapy";
run;

proc format;
value celltype
1="Squamous"
2="Smallcell"
3="Adeno"
4="Large";
run;

proc format;
value prior
0="No Prior Therapy"
10="Prior Therapy";
run;

/*Check the data with formatting*/
proc print data=vet;
format trt treatment.;
format celltype celltype.;
format prior prior.;
run;

/*Explore the data*/
proc lifetest data=vet plots=survival();
time time*status(0);
format trt treatment.;
strata trt; 
run;

/*----- Check Martingale Residuals to verify linear form of continuous covariates -----*/
/*Fit a Cox PH model with no covariates and save the Martingale residuals*/
proc phreg data=vet;
class trt celltype karno prior;
model time*status(0) = ;
output out=Outp resmart=Mart;
run;

/*Plot the Martingale residual plots for the continuous covariates to assess fitted shape*/
proc loess data = Outp plots=ResidualsBySmooth(smooth);
model Mart = diagtime/smooth=0.2 0.4 0.6 0.8;
run;

proc loess data = Outp plots=ResidualsBySmooth(smooth);
model Mart = age/smooth=0.2 0.4 0.6 0.8;
run;

/*Verify functional shape using ASSESS statement, since residual plots were inconclusive*/
proc phreg data=vet;
class trt celltype karno prior;
model time*status(0) = trt celltype karno diagtime age prior;
assess var=(diagtime) / resample seed=123;
run;

proc phreg data=vet;
class trt celltype karno prior;
model time*status(0) = trt celltype karno diagtime age prior;
assess var=(age) / resample seed=123;
run;

/*Fit a Cox PH model using the linear form of diagtime and age based on prior analysis of Martingale residuals*/
proc phreg data=vet;
class trt celltype karno prior;
model time*status(0) = trt celltype karno diagtime age prior;
output out=Outp xbeta=Xb resmart=Mart ressch= schtrt schcelltype schkarno schdiagtime schage schprior;
run;

/*Double check Martingale residuals to ensure correct functional form*/
proc loess data = Outp plots=ResidualsBySmooth(smooth);
model Mart = diagtime/smooth=0.2 0.4 0.6 0.8;
run;

proc loess data = Outp plots=ResidualsBySmooth(smooth);
model Mart = age/smooth=0.2 0.4 0.6 0.8;
run;

/*Evaluate Schoenfeld residuals for each covariate to check proportional hazard assumption*/
proc loess data = Outp;
model schtrt=time / smooth=(0.2 0.4 0.6 0.8);
run;

proc loess data = Outp;
model schcelltype=time / smooth=(0.2 0.4 0.6 0.8);
run;

proc loess data = Outp;
model schkarno=time / smooth=(0.2 0.4 0.6 0.8);
run;

proc loess data = Outp;
model schdiagtime=time / smooth=(0.2 0.4 0.6 0.8);
run;

proc loess data = Outp;
model schage=time / smooth=(0.2 0.4 0.6 0.8);
run;

proc loess data = Outp;
model schprior=time / smooth=(0.2 0.4 0.6 0.8);
run;

/*Verify the proportional hazard assumption using ASSESS statement*/
proc phreg data=vet;
class trt celltype karno prior;
model time*status(0) = trt celltype karno diagtime age prior;
assess ph / resample seed=1234;
run;

/*Verify that removing karno satisfies the proportional hazard assumption*/
proc phreg data=vet;
class trt celltype prior;
model time*status(0) = trt celltype diagtime age prior;
assess ph / resample seed=999;
run;

/*Determine significant covariates in the model*/ 
proc phreg data=vet;
class trt celltype prior;
model time*status(0) = trt celltype diagtime age prior;
run;

/*Begin backward selection process for model*/
proc phreg data=vet;
class trt celltype;
model time*status(0) = trt celltype diagtime age;
run;

proc phreg data=vet;
class trt celltype;
model time*status(0) = trt celltype diagtime;
run;

/*Create the reduced model with the significant covariates*/
proc phreg data=vet;
class trt celltype;
model time*status(0) = trt celltype;
run;
