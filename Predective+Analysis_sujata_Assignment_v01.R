setwd("C:/Upgrad/Predective Analysis")
## Business Understanding###

# Attribute: Attribute Range  https://archive.ics.uci.edu/ml/datasets/Automobile

# loading the required libraries

library(data.table)
library(dplyr)
require(ggplot2)
require(stringr)
library(DT)
require(tidyr)
require(corrplot)
library(scales)
library(MASS)
library(car)

##Data understanding 

carPrice <- read.csv("CarPrice_Assignment.csv", header = T, stringsAsFactors = FALSE)

View(carPrice)
# structure of data shows the data types
str(carPrice)
summary(carPrice)

#checking first few records
head(carPrice)

#Check for duplicate values
sum(duplicated(carPrice$car_ID)) ## 0 i.e. there are no dublicate id

#Check for NA values
sum(is.na(carPrice$car_ID)) ## 0 NA

missing_values <- carPrice %>%
  summarise_all(funs(sum(is.na(.))/n()))

View(missing_values) # 0 i.e. no NAs

# Extracting word before " " for each sector in the new frame

# creating a vector with Company Name and storing the splited company name and car model in new vector 
#splited_company_model <- str_split_fixed(carPrice$CarName, "[ ]", 2)

carPrice <- separate(carPrice, col = CarName, into = c("Company","Model"), sep = " ")

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')


##############Dummy variable######################

# Categorical variable with 2 level
#fueltype   : diesel, gas
#aspiration : std, turbo. 
#doornumber	: four, two. 
#enginelocation : front, rear.

############# FUEL TYPE #######################
str(carPrice$fueltype)
carPrice$fueltype <- factor(carPrice$fueltype)
summary(factor(carPrice$fueltype))

# One simple way to convert Fuel Type variable to numeric is to replace the levels- Gas's and Diesel's with 1 and 0 is:
levels(carPrice$fueltype)<-c(0,1)

carPrice$fueltype<- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

summary(factor(carPrice$fueltype))
############# FUEL TYPE #######################

# Do the same for other such categorical variables

############# ASPIRATION #######################

str(carPrice$aspiration)
carPrice$aspiration <- factor(carPrice$aspiration)
summary(factor(carPrice$aspiration))

# One simple way to convert aspiration variable to numeric is to replace the levels- turbo's and std's with 1 and 0 is:
levels(carPrice$aspiration)<-c(0,1)

carPrice$aspiration<- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

summary(factor(carPrice$aspiration))
############# ASPIRATION #######################

############# Doornumber #######################

str(carPrice$doornumber)
carPrice$doornumber <- factor(carPrice$doornumber)
summary(factor(carPrice$doornumber))

# One simple way to convert doornumber variable to numeric is to replace the levels- two's and four's with 1 and 0 is:
levels(carPrice$doornumber)<-c(0,1)

carPrice$doornumber<- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

summary(factor(carPrice$doornumber))
############# Doornumber #######################

############# Engine Location #######################

str(carPrice$enginelocation)
carPrice$enginelocation <- factor(carPrice$enginelocation)
summary(factor(carPrice$enginelocation))

# One simple way to convert doornumber variable to numeric is to replace the levels- rear's and front's with 1 and 0 is:
levels(carPrice$enginelocation)<-c(0,1)

carPrice$enginelocation<- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

summary(factor(carPrice$enginelocation))
############# Engine Location ########################

# Now we can check variables having more than 3 levels.
#drivewheel		: 4wd, fwd, rwd. 
#body-style   : hardtop, wagon, sedan, hatchback, convertible. 
#enginetype		: dohc, dohcv, l, ohc, ohcf, ohcv, rotor. 
#num-of-cylinders: eight, five, four, six, three, twelve, two
#fuel-system: 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi.
#make: alfa-romero, audi, bmw, chevrolet, dodge, honda, isuzu, jaguar, mazda, mercedes-benz, mercury, 
#mitsubishi, nissan, peugot, plymouth, porsche, renault, saab, subaru, toyota, volkswagen, volvo 


################## Drive wheel#############################
summary(factor(carPrice$drivewheel))

#Converting "drivewheel" into dummies . 
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = carPrice))

#check the dummy_1 data frame.
View(dummy_drivewheel)

#This column should be removed from the newly created dummy_drivewheel dataframe containing the dummy values for the variable "drivewheel". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
carPrice_1 <- cbind(carPrice[,-9], dummy_drivewheel)
View(carPrice_1)


################## car Body #############################
summary(factor(carPrice_1$carbody))

#Converting "carbody" into dummies . 
dummy_carbody <- data.frame(model.matrix( ~carbody, data = carPrice_1))

#check the dummy_1 data frame.
View(dummy_carbody)

#This column should be removed from the newly created dummy_carbody dataframe containing the dummy values for the variable "carbody". 
dummy_carbody <- dummy_carbody[,-1]

carPrice_2 <- cbind(carPrice_1[,-8], dummy_carbody)
View(carPrice_2)
###################### Car Body##################################

################## Engine type #############################
summary(factor(carPrice_2$enginetype))

#Converting "carbody" into dummies . 
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carPrice_2))

#check the dummy_1 data frame.
View(dummy_enginetype)

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "enginetype". 
dummy_enginetype <- dummy_enginetype[,-1]

carPrice_3 <- cbind(carPrice_2[,-14], dummy_enginetype)
View(carPrice_3)
###################### Engine type ##################################

################### Cylinder Number #############################
summary(factor(carPrice_3$cylindernumber))

#Converting "cylindernumber" into dummies . 
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = carPrice_3))

#check the dummy_1 data frame.
View(dummy_cylindernumber)

#This column should be removed from the newly created dummy_cylindernumber dataframe containing the dummy values for the variable "cylindernumber". 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

carPrice_4 <- cbind(carPrice_3[,-14], dummy_cylindernumber)
View(carPrice_4)
###################### Cylinder Number ##################################

################## Fuel System #############################
summary(factor(carPrice_4$fuelsystem))

#Converting "fuelsystem" into dummies . 
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carPrice_4))

#check the dummy_1 data frame.
View(dummy_fuelsystem)

#This column should be removed from the newly created dummy_fuelsystem dataframe containing the dummy values for the variable "fuelsystem". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

carPrice_5 <- cbind(carPrice_4[,-15], dummy_fuelsystem)

View(carPrice_5)
###################### Fuel System ##################################

################## correcting company Names #############################
sum(duplicated(carPrice_5))


levels(as.factor(carPrice_5$Company))

carPrice_5$Company<-ifelse(carPrice_5$Company=="maxda", "mazda", 
                           ifelse(carPrice_5$Company=="Nissan", "nissan", 
                                  ifelse(carPrice_5$Company=="porcshce", "porsche", 
                                         ifelse(carPrice_5$Company=="toyouta", "toyota", 
                                                ifelse(carPrice_5$Company=="vokswagen", "volkswagen", 
                                                       ifelse(carPrice_5$Company=="vw", "volkswagen",
                                                              ifelse(carPrice_5$Company=="alfa-romero", "alfa-romeo",
                                                                     carPrice_5$Company)))))))
sum(duplicated(carPrice_5))

str(carPrice_5$Company)
summary(carPrice_5$Company)

summary(factor(carPrice_5$Company))



#Converting "Company" into dummies . 
dummy_Company <- data.frame(model.matrix( ~Company, data = carPrice_5))

#check the dummy_1 data frame.
View(dummy_Company)

#This column should be removed from the newly created dummy_Company dataframe containing the dummy values for the variable "Company". 
dummy_Company<-dummy_Company[,-1]

carPrice_6 <- cbind(carPrice_5[,-3], dummy_Company)

View(carPrice_6)
###################### company Name ##################################

################Symbolings###########################333
summary(as.factor(carPrice_6$symboling))

#Binning the Symbolings
#high(2,3),mid(0,1),low(-1,-2)
carPrice_6$symboling<-as.factor(carPrice_6$symboling)

levels(carPrice_6$symboling)[1:2] <- "low"
levels(carPrice_6$symboling)[2:3] <- "mid"
levels(carPrice_6$symboling)[3:4] <- "high"

summary(factor(carPrice_6$symboling))

#Converting "symboling" into dummies . 
dummy_symboling <- data.frame(model.matrix( ~symboling, data = carPrice_6))

#check the dummy_1 data frame.
View(dummy_symboling)

#This column should be removed from the newly created dummy_symboling dataframe containing the dummy values for the variable "symboling". 
dummy_symboling <- dummy_symboling[,-1]

carPrice_7 <- cbind(carPrice_6[,-2], dummy_symboling)

View(carPrice_7)


###############Outlier checks####################

#Performing outlier checks on below Numeric variables
# wheel-base: continuous from 86.6 120.9. 
# length: continuous from 141.1 to 208.1. 
# width: continuous from 60.3 to 72.3. 
# height: continuous from 47.8 to 59.8. 
# curb-weight: continuous from 1488 to 4066.
# engine-size: continuous from 61 to 326. 
# bore: continuous from 2.54 to 3.94. 
# stroke: continuous from 2.07 to 4.17. 
# compression-ratio: continuous from 7 to 23. 
# horsepower: continuous from 48 to 288. 
# peak-rpm: continuous from 4150 to 6600. 
# city-mpg: continuous from 13 to 49. 
# highway-mpg: continuous from 16 to 54. 
# price: continuous from 5118 to 45400.


# On wheelbase (Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$wheelbase, main="Wheel base", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$wheelbase )$out)) 
quantile(carPrice_7$wheelbase, seq(0,1,0.01))

# Note that there is a jump on 100% and hence caping values above 115.544 i.e. 120.900 is 100%.

carPrice_7$wheelbase[which(carPrice_7$wheelbase>115.544)]<-115.544

# On carlength(no Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$carlength, main="car length", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$carlength )$out)) 
quantile(carPrice_7$carlength, seq(0,1,0.01))


# On carwidth(no Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$carwidth, main="car width", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$carwidth )$out)) 
quantile(carPrice_7$carwidth, seq(0,1,0.01))

# On carheight(no Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$carheight, main="car height", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$carheight )$out)) 
quantile(carPrice_7$carheight, seq(0,1,0.01))

# On curbweight(Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$curbweight, main="car weight", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$curbweight )$out)) 
quantile(carPrice_7$curbweight, seq(0,1,0.01))

# On engine size(Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$enginesize, main = "engine size ", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$enginesize )$out)) 
quantile(carPrice_7$enginesize, seq(0,1,0.01))

#there is a jump between 32 to 33 % and 49 to 50% and 93 to 94 %. from this we can consider caping values above 95% as floor value

carPrice_7$enginesize[which(carPrice_7$enginesize>201.20)]<-201.20

# On boreratio(Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$boreratio, main="bore ratio", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$boreratio )$out)) 
quantile(carPrice_7$boreratio, seq(0,1,0.01))


# On stroke(Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$stroke, main="stroke", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$stroke )$out)) 
quantile(carPrice_7$stroke, seq(0,1,0.01))
#There is jump between 0 to 1%

# On compressionratio(Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$compressionratio, main = "compression ratio ", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$compressionratio )$out)) 
quantile(carPrice_7$compressionratio, seq(0,1,0.01))


# On horsepower(Outliers) 
#Using Box plot for identiying outliers
boxplot(carPrice_7$horsepower, main="horse power", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$horsepower)$out)) 
quantile(carPrice_7$horsepower, seq(0,1,0.01))
#There is jump at 97 to 98 % hence caping above 97%

carPrice_7$horsepower[which(carPrice_7$horsepower>184.00)]<-184.00


# On peakrpm (no outliers)
boxplot(carPrice_7$peakrpm, main="peakrpm", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$peakrpm)$out)) 
quantile(carPrice_7$peakrpm,seq(0,1,0.01))

# On citympg(outliers)
boxplot(carPrice_7$citympg, main="citympg", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$citympg)$out)) 
quantile(carPrice_7$citympg,seq(0,1,0.01))

#Note that there is a jump on 99% and 100% Therefore, we cap all values above 38.0 (98%) to 38.0. 
carPrice_7$citympg[which(carPrice_7$citympg>38.0)]<-38.0

# On highwaympg(no outliers)
boxplot(carPrice_7$highwaympg, main="highwaympg", sub=paste("Outlier rows: ", boxplot.stats(carPrice_7$highwaympg)$out)) 
quantile(carPrice_7$highwaympg,seq(0,1,0.01))
#########################Finish Outlier for Numeric variable##########

################## Modelling #####################

# Scatter plot used check the relation with car_ID and price
scatter.smooth(x=carPrice_7$car_ID, y=carPrice_7$price, main="Price ~ car_ID")
carPrice_7 <- carPrice_7[,-1]

#Removing Model type from DF
carPrice_clean_DF <- carPrice_7[,-1]

#Structure of the DF
str(carPrice_clean_DF)

#creating test and training data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carPrice_clean_DF), 0.7*nrow(carPrice_clean_DF))
# generate the train data set
train = carPrice_clean_DF[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carPrice_clean_DF[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
carPrice_Model_1 <-lm(price~.,data=train)

summary(carPrice_Model_1)

#Multiple R-squared:  0.9779,	Adjusted R-squared:  0.9643 
#F-statistic: 72.07 on 54 and 88 DF,  p-value: < 2.2e-16

step <- stepAIC(carPrice_Model_1, direction="both")


carPrice_Model_2 <- lm(formula =price ~ fueltype + aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight + stroke + compressionratio + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                         Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault + Companysaab + Companytoyota + Companyvolkswagen + 
                         Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_2)

#Multiple  R-squared:  0.9764,	Adjusted R-squared:  0.9687  
#F-statistic: 109.5 on 39 and 103 DF,  p-value: < 2.2e-16

# Pass the model_1 in the vif function
vif(carPrice_Model_2)

#Removing compressionratio based on higher vif and insignificant p value

carPrice_Model_3 <- lm(formula =price ~ fueltype + aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight + stroke  + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                         Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault + Companysaab + Companytoyota + Companyvolkswagen + 
                         Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_3)

#Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9679  
#F-statistic: 127.7 on 34 and 108 DF,  p-value: < 2.2e-16

vif(carPrice_Model_3)


#Removing fueltype

carPrice_Model_4 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight + stroke  + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                         Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault + Companysaab + Companytoyota + Companyvolkswagen + 
                         Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_4)
#Multiple  R-squared:  0.9753,	Adjusted R-squared:  0.9678 
#F-statistic: 106.5 on 33 and 109 DF,  p-value: < 2.2e-16
#We are good with the R sq and adjusted R sq

vif(carPrice_Model_4)

#Removing stroke

carPrice_Model_5 <-lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                        carwidth + curbweight   + carbodyhardtop + 
                        carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                        enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                        cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                        Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                        Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                        Companyrenault + Companysaab + Companytoyota + Companyvolkswagen + 
                        Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_5)

#Multiple R-squared:  0.9748,	Adjusted R-squared:  0.9675 
#F-statistic: 38.36 on 32 and 110 DF,  p-value: < 2.2e-16
#No Significant reduction in the R square and adjusted R square

vif(carPrice_Model_5)

#Removing Companysaab

carPrice_Model_6 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                         Companyhonda + Companyisuzu + Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault  + Companytoyota + Companyvolkswagen + 
                         Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_6)
#Multiple R-squared:  0.9746,	Adjusted R-squared:  0.9675
vif(carPrice_Model_6)


#Removing Companyhonda
carPrice_Model_7 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                          Companyisuzu + Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault  + Companytoyota + Companyvolkswagen + 
                         Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_7)
# R-squared:  0.9738,	Adjusted R-squared:  0.9667

vif(carPrice_Model_7)

#Removing Companyisuzu
carPrice_Model_8 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                          Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault  + Companytoyota + Companyvolkswagen + 
                         Companyvolvo + enginetypeohc, data = train)
summary(carPrice_Model_8)
#R-squared:  0.9736,	Adjusted R-squared:  0.9668 
vif(carPrice_Model_8)
#Removing Companyvolvo
carPrice_Model_9 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                         Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi + Companynissan + Companyplymouth + 
                         Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_9)
#R-squared:  0.9736,	Adjusted R-squared:  0.9671 
vif(carPrice_Model_9)
#Removing Companynissan
carPrice_Model_10 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                         Companyjaguar + Companymazda + 
                         Companymercury + Companymitsubishi   + Companyplymouth + 
                         Companyrenault  + Companytoyota + Companyvolkswagen + 
                         enginetypeohc, data = train)
summary(carPrice_Model_10)
# R-squared:  0.9732,	Adjusted R-squared:  0.9669 
vif(carPrice_Model_10)

#Removing Companymercury
carPrice_Model_11 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                          carwidth + curbweight   + carbodyhardtop + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                          Companyjaguar + Companymazda + 
                           Companymitsubishi   + Companyplymouth + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_11)
# R-squared:  0.973,	Adjusted R-squared:  0.9669  
vif(carPrice_Model_11)
#Removing carbodyhardtop
carPrice_Model_12 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                          carwidth + curbweight    + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                          Companyjaguar + Companymazda + 
                          Companymitsubishi   + Companyplymouth + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_12)
# R-squared:  0.9717,	Adjusted R-squared:  0.9656 
vif(carPrice_Model_12)


#Removing wheelbase
carPrice_Model_13 <- lm(formula =price ~  aspiration + enginelocation  + 
                          carwidth + curbweight    + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick + Companydodge + 
                          Companyjaguar + Companymazda + 
                          Companymitsubishi   + Companyplymouth + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_13)
#R-squared:  0.9704,	Adjusted R-squared:  0.9644
vif(carPrice_Model_13)

#Removing Companydodge
carPrice_Model_14 <- lm(formula =price ~  aspiration + enginelocation  + 
                          carwidth + curbweight    + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar + Companymazda + 
                          Companymitsubishi   + Companyplymouth + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_14)
#R-squared:  0.9696,	Adjusted R-squared:  0.9637
vif(carPrice_Model_14)

#Removing Companyplymouth
carPrice_Model_15 <- lm(formula =price ~  aspiration + enginelocation  + curbweight+
                          carwidth +carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar + Companymazda +  Companymitsubishi + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_15)
#R-squared:  0.9688,	Adjusted R-squared:  0.9631 
vif(carPrice_Model_15)

#Removing carbodysedan
carPrice_Model_16 <- lm(formula =price ~  aspiration + enginelocation  + curbweight+
                          carwidth +carbodyhatchback  + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar + Companymazda +  Companymitsubishi + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_16)
#R-squared:  0.9682,	Adjusted R-squared:  0.9627
vif(carPrice_Model_16)

#Removing carbodyhatchback
carPrice_Model_17 <- lm(formula =price ~  aspiration + enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar + Companymazda +  Companymitsubishi + 
                          Companyrenault  + Companytoyota + Companyvolkswagen + 
                          enginetypeohc, data = train)
summary(carPrice_Model_17)
#R-squared:  0.9676,	Adjusted R-squared:  0.9622
vif(carPrice_Model_17)

#Removing aspiration
carPrice_Model_18 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar + Companymazda +  Companymitsubishi + Companyvolkswagen+
                          Companyrenault  + Companytoyota  + 
                          enginetypeohc, data = train)
summary(carPrice_Model_18)
#R-squared:  0.9667,	Adjusted R-squared:  0.9616 
vif(carPrice_Model_18)

#Removing Companymazda
carPrice_Model_19 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +  Companymitsubishi + Companyvolkswagen+
                          Companyrenault  + Companytoyota  +
                          enginetypeohc, data = train)
summary(carPrice_Model_19)
#R-squared:  0.9651,	Adjusted R-squared:  0.9601
vif(carPrice_Model_19)

#Removing Companyvolkswagen
carPrice_Model_20 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +  Companymitsubishi + 
                          Companyrenault  + Companytoyota  + 
                          enginetypeohc, data = train)
summary(carPrice_Model_20)
#R-squared:   0.9643,	Adjusted R-squared:  0.9595 
vif(carPrice_Model_20)

#Removing carbodywagon
carPrice_Model_21 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +  Companymitsubishi + 
                          Companyrenault  + Companytoyota  + 
                          enginetypeohc, data = train)
summary(carPrice_Model_21)
#R-squared:  0.9626,	Adjusted R-squared:  0.9579 
vif(carPrice_Model_21)

#Removing Companyrenault
carPrice_Model_22 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +  Companymitsubishi + 
                           Companytoyota  + 
                          enginetypeohc, data = train)
summary(carPrice_Model_22)
#R-squared:  0.9609,	Adjusted R-squared:  0.9562
vif(carPrice_Model_22)

#Removing Companymitsubishi
carPrice_Model_23 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +   
                          Companytoyota  + 
                          enginetypeohc, data = train)
summary(carPrice_Model_23)
#R-squared:  0.9594,	Adjusted R-squared:  0.955
vif(carPrice_Model_23)

#Removing Companytoyota
carPrice_Model_24 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +enginetypeohc, data = train)
summary(carPrice_Model_24)
#R-squared:  0.9565,	Adjusted R-squared:  0.9521 
vif(carPrice_Model_24)

cor(train$curbweight,train$carwidth)

cor(train$enginetypeohc,train$enginelocation)

#Removing carwidth
carPrice_Model_25 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive  + cylindernumberfour+
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  +enginetypeohc, data = train)
summary(carPrice_Model_25)
#R-squared:  0.9428,	Adjusted R-squared:  0.9375
vif(carPrice_Model_25)

#Removing enginetypeohc
carPrice_Model_26 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive  + cylindernumberfour+
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  , data = train)
summary(carPrice_Model_26)
#R-squared:  0.9396,	Adjusted R-squared:  0.9345
vif(carPrice_Model_26)

#Removing enginetypeohcf
carPrice_Model_27 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                           enginetyperotor + cylindernumberfive  + cylindernumberfour+
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  , data = train)
summary(carPrice_Model_27)
#R-squared:  0.939,	Adjusted R-squared:  0.9344
vif(carPrice_Model_27)

#Removing enginetyperotor
carPrice_Model_28 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                           cylindernumberfive  +cylindernumberfour+
                          cylindernumbersix + Companybmw + Companybuick  + 
                          Companyjaguar  , data = train)
summary(carPrice_Model_28)
#R-squared:  0.9257,	Adjusted R-squared:  0.9207
vif(carPrice_Model_28)

#Removing Companyjaguar
carPrice_Model_29 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          cylindernumberfive  +cylindernumberfour+
                          cylindernumbersix + Companybmw + Companybuick  
                            , data = train)
summary(carPrice_Model_29)
#R-squared:    0.9199,	Adjusted R-squared:  0.9151
vif(carPrice_Model_29)

#Removing cylindernumbersix
carPrice_Model_30 <- lm(formula =price ~   enginelocation+curbweight   + enginetypel+
                          cylindernumberfour+cylindernumberfive+
                           Companybmw + Companybuick  
                        , data = train)
summary(carPrice_Model_30)
#R-squared:  0.9104,	Adjusted R-squared:  0.9057 
vif(carPrice_Model_30)

#Removing enginetypel
carPrice_Model_31 <- lm(formula =price ~   enginelocation+curbweight+
                          cylindernumberfour+cylindernumberfive+
                          Companybmw + Companybuick  
                        , data = train)
summary(carPrice_Model_31)
# R-squared:  0.9045,	Adjusted R-squared:  0.9003 
vif(carPrice_Model_31)

#Removing cylindernumberfive
carPrice_Model_32 <- lm(formula =price ~   enginelocation+curbweight+
                          cylindernumberfour+
                          Companybmw + Companybuick  
                        , data = train)
summary(carPrice_Model_32)
# R-squared:  0.8964,	Adjusted R-squared:  0.8927

vif(carPrice_Model_32)


# Predict the house prices in the testing dataset with carPrice_Model_32
Predict_1 <- predict(carPrice_Model_32,test[,-10])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

############### carPrice_Model_31###############
# Predict the house prices in the testing dataset with carPrice_Model_31
Predict_1 <- predict(carPrice_Model_31,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

############### carPrice_Model_30###############
# Predict the house prices in the testing dataset with carPrice_Model_30
Predict_1 <- predict(carPrice_Model_30,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

##############Final Model is carPrice_Model_32###########################
#Final variable- enginelocation+curbweight+cylindernumberfour+Companybmw + Companybuick

##Evaluating the model with the dataset####

carPrice_Model_Test <- lm(formula =price ~   enginelocation+curbweight+cylindernumberfour+Companybmw + Companybuick  
                        , data = carPrice_clean_DF)
par(mfrow=c(2,2))
plot(carPrice_Model_Test)


# Here is our observation on different generated graphs
# 1. Residuals vs Fitted :-We could find equally spread residuals around a linear line without distinct patterns, so it is a good indication that it does not have non-linear relationships

# 2. Normal Q-Q :- It's good residuals are lined well on the straight dashed line.

# 3. Scale-Location :- It's good we could see a horizontal line with equally (randomly) spread points.

# 4. Residuals vs Leverage :- The plot identified the influential observation on #17 and #75 rows where price could have outliers so Lets remove those remove and run our test again

#Need to Residuals vs Leverage as there are outliers

#checking outliers on price

#Using Box plot for identiying outliers
boxplot(carPrice_clean_DF$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(carPrice_clean_DF$price)$out))
quantile(carPrice_clean_DF$price, seq(0,1,0.01))

#Removing 17 and 75 from df
carPrice_clean_DF <- carPrice_clean_DF[-c(17, 75), ] 

#checking the plot for retesting

carPrice_Model_Test <- lm(formula =price ~   enginelocation+curbweight+cylindernumberfour+Companybmw + Companybuick  
                          , data = carPrice_clean_DF)
par(mfrow=c(2,2))
plot(carPrice_Model_Test)


#Re -creating the test data as the graph looks fine

set.seed(100)
trainindices= sample(1:nrow(carPrice_clean_DF), 0.7*nrow(carPrice_clean_DF))
train1 = carPrice_clean_DF[trainindices,]
test1 = carPrice_clean_DF[-trainindices,]

CarPrice_model_final <- lm(formula = price ~   enginelocation+curbweight+cylindernumberfour+Companybmw + Companybuick, 
                  data = train1)

summary(CarPrice_model_final)

Predict_1 <- predict(CarPrice_model_final,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared

#Final Model- R-squared:  0.8962,	Adjusted R-squared:  0.8924
#test RSq- 0.832
# Here deviation is more than (~5%) so  it is always a subjective call to take