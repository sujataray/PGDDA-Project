#Load required libraries
library(dplyr)
library(ggplot2) 
require(ggplot2)
library(stringr)
library(tidyr)
require(scales)

setwd("~/Data Science/EDA Assignment/R file")
#Import the data
Loan_Details<- read.csv("loan.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))

#View the data - take a glance of it
View(Loan_Details)

#Check the data
str(Loan_Details)

#We observe that there are many columns that have NA across rows.
#Find them and remove as they don't add any value for analysis.
#Logic here is if sum of NA is equal to rows count then all entries are NA.

#So remove such columns
Loan_Details <- Loan_Details[, colSums(is.na(Loan_Details)) != nrow(Loan_Details)]
View(Loan_Details)

#We see that many of the rows does not help in analysis and they are irrelevant:
#We can remove such fields - following fields we can remove:
#id, member_id, ... so we will create a vector of these fields and removed them:
#removing application type also, as all values are "INDIVIDUAL" TYPE and similar fields where
#we see only one type of values.
#This is first level clean-up, later based on further anlysis we can remove more fields that
#are not useful for analysis.
#As we can see we are removing loan amout and funded_amnt_inv - as these will not help 
# we are considering only funded_amnt as this is the only amount that bank is lending to the 
# the customers.
unrelevant_fields <- c("member_id","id","pymnt_plan","sub_grade","emp_title",
                       "url","desc","title","zip_code","addr_state", "application_type",
                       "initial_list_status","delinq_amnt","pub_rec_bankruptcies", 
                       "delinq_2yrs","earliest_cr_line","inq_last_6mths","open_acc","pub_rec", 
                       "revol_bal", "revol_util","total_acc","out_prncp","out_prncp_inv",
                       "total_pymnt", "total_pymnt_inv","total_rec_prncp","total_rec_int",
                       "total_rec_late_fee", "recoveries","collection_recovery_fee",
                       "last_pymnt_d","last_pymnt_amnt","next_pymnt_d","last_credit_pull_d",
                       "loan_amnt", "funded_amnt_inv", "collections_12_mths_ex_med",
                       "chargeoff_within_12_mths", "tax_liens", "policy_code",
                       "acc_now_delinq")

#We will remove these columns from the data frame
Loan_Modified <- Loan_Details[,!(colnames(Loan_Details) %in% unrelevant_fields)]

#Verify NA presence
Loan_Modified %>%summarise_all(funs(sum(is.na(.))))

sum(is.na(Loan_Modified))
#We see that there are significant NA values exists in the table
#Such high numbers will not help in analysis or may give wrong analysis.
#Hence we remove such columns where more than 20% are NA for better analysis.

Loan_Data <- Loan_Modified[ , colMeans(is.na(Loan_Modified)) < 0.20 ]

#Check if all NAs has been cleared or not
sum(is.na(Loan_Data)) # 0 NAs
View(Loan_Data)

#We see that loan status of "Current" will not help as we cannot decide based on that 
#whether it is a risky or non risky one 
Loan_Data <- filter(Loan_Data,loan_status %in% c("Fully Paid","Charged Off"))

#We notice that date format is not correct
# Converting this variable to profer date format 
Loan_Data$issue_d <- paste("01-",Loan_Data$issue_d,sep="")
Loan_Data$issue_d <- as.Date(Loan_Data$issue_d,"%d-%B-%y")

str(Loan_Data)

#We will convert all the character type variables to factor for better analysis
Loan_Data[sapply(Loan_Data, is.character)] <- lapply(Loan_Data[sapply(Loan_Data, is.character)], 
                                                     as.factor)

str(Loan_Data)

#Segmentation / Sub-grouging for better visualizations / Adding Derived Variables to the table
#We see that the funded loan amount, int rate, emi, experience, salary range and experience range are wide.
#So we better group them under different ranges for better analysis - derived variables.

#we will start with funded loan amount categorization
str(Loan_Data$funded_amnt)
mean(Loan_Data$funded_amnt)
range(Loan_Data$funded_amnt)
#We see that this value ranges from 500 to 3500 and the mean is 1500.
#With such a huge range it is difficult to analyze properly. So we will categorize them into 4 buckets/bins:
Loan_Data$bin_funded_amnt<-ifelse(Loan_Data$funded_amnt<=5000,"Small",
                                  ifelse(Loan_Data$funded_amnt>5000 & Loan_Data$funded_amnt<=15000,"Medium",
                                         ifelse(Loan_Data$funded_amnt>15000 & Loan_Data$funded_amnt<=25000,"High",
                                                "VeryHigh")))

#creating groups for interest rate
#First let us convert int rate from % to numeric (though extract_numeric is deprecated - it works)
Loan_Data$int_rate <- extract_numeric(Loan_Data$int_rate)
str(Loan_Data$int_rate)
mean(Loan_Data$int_rate)
range(Loan_Data$int_rate)
#We see that this value ranges from 5.42 to 24.40 and the mean is 11.93
#So we will create bucket for the same:
Loan_Data$bin_int_rate<-ifelse(Loan_Data$int_rate<=10,"Low_rate",
                               ifelse(Loan_Data$int_rate>10 & Loan_Data$int_rate<=15,
                                      "Medium_rate","High_rate"))


#Since we created  an interest rate range above - we are not using the grade field (A, B, C..)
#We will remove it
Loan_Data <- Loan_Data[,!(colnames(Loan_Data) %in% c("grade"))]


#Creating groups for installment or EMI
str(Loan_Data$installment)
mean(Loan_Data$installment)
range(Loan_Data$installment)
#We see that this value ranges from 15.69 to 1305.19 and the mean is 322.46.
#So we will create bucket for the same:
Loan_Data$bin_installment<-ifelse(Loan_Data$installment<=200,"Small",
                                  ifelse(Loan_Data$installment>200 & Loan_Data$installment<=400,"Medium",
                                         ifelse(Loan_Data$installment>400 & Loan_Data$installment<=600,"High","VeryHigh")))

#Creating groups for Annual income of customers
str(Loan_Data$annual_inc)
mean(Loan_Data$annual_inc)
range(Loan_Data$annual_inc)
#We see that this value ranges from 400 to  600000 to 24.40 and the mean is 68778
#So we will create bucket for the same:
Loan_Data$bin_annual_inc<-ifelse(Loan_Data$annual_inc<=50000,"Small",
                                 ifelse(Loan_Data$annual_inc>50000 & Loan_Data$annual_inc<=100000,"Medium",
                                        ifelse(Loan_Data$annual_inc>100000 & Loan_Data$annual_inc<=150000,"High","VeryHigh")))


#Creating groups for experience (emp length)
#Few rows are NA in emp length - better to remove them
Loan_Data$emp_length <- extract_numeric(Loan_Data$emp_length)
str(Loan_Data$emp_length)
Loan_Data$bin_emp_length <- as.factor(ifelse(Loan_Data$emp_length<=1,"Freshers",
                                             ifelse(Loan_Data$emp_length>1&Loan_Data$emp_length<=3,"Junior",
                                                    ifelse(Loan_Data$emp_length>3&Loan_Data$emp_length<=7,"Senior","Expert"))))

#Creating year group as an additional column for yearly views
Loan_Data$year <- as.factor(format(Loan_Data$issue_d,"%Y"))

#We will carry out some univariate Analyis
#where we use single variable and also use segmentated single variable (Segmented Uniariate analysis)
#With this we will try to understand the influence and bhehaviour of each variable.

#Creating a x axis constant for better styling
x_axis_text_style <- element_text(angle = 60, hjust = 1,face = "bold.italic", color = "black", size = 9)


#Applications distribution yearwise
ggplot(Loan_Data, aes(x=factor(Loan_Data$year)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Yearwise ", y = "Percentage", x= "Year")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for Loan Amount wise
ggplot(Loan_Data, aes(x=factor(Loan_Data$bin_funded_amnt)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Loan Amount ", y = "Percentage", x= "Loan Amount")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for Interest Rate
ggplot(Loan_Data, aes(x=factor(Loan_Data$bin_int_rate)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Interest Rate ", y = "Percentage", x= "Interest Rate")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for Income range
ggplot(Loan_Data, aes(x=factor(Loan_Data$bin_annual_inc)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Income Range ", y = "Percentage", x= "Income Range")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for EMI / Installment range
ggplot(Loan_Data, aes(x=factor(Loan_Data$bin_installment)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Installment ", y = "Percentage", x= "Installment")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for Experience Range
ggplot(Loan_Data, aes(x=factor(Loan_Data$bin_emp_length)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Experience Range ", y = "Percentage", x= "Experience Range")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for Loan term
ggplot(Loan_Data, aes(x=factor(Loan_Data$term)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Loan Term ", y = "Percentage", x= "Loan Term")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for Homeownership
ggplot(Loan_Data, aes(x=factor(Loan_Data$home_ownership)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - HouseOwnership ", y = "Percentage", x= "HouseOwnership")+ theme(axis.text.x = x_axis_text_style)

#Application Distribution for verification
ggplot(Loan_Data, aes(x=factor(Loan_Data$verification_status)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Verification ", y = "Percentage", x= "Verification Status")+ theme(axis.text.x = x_axis_text_style)

#Applications Distribution for puprose
ggplot(Loan_Data, aes(x=factor(purpose)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Purpose ", y = "Percentage", x= "Loan Purpose")+ theme(axis.text.x = x_axis_text_style)
##remove purpose and keep only 4 or 5 based on impact (Charged is negligible)
#From the above plot it is clear that the 
# 46.8% - Application for Debt_consolidation 
# 13.0%   - Application for the credit card
# 10.0% - Application for other purpose
# 7.5%  - Application for Home improvement loan 
# 5.6%  - Application for major purchase loan

#Based on loan purpose distribution, further anlysis will be done on the categories greater than 5%
# Debt_consolidation
# credit_card
# home_improvement
# major_purchase
# other ## we don't know what kind of loan purpose comes under "other" category.

#Hence modifying Dataframe with above mentioned purpose only
Loan_Data <- filter(Loan_Data,purpose %in% c("credit_card","debt_consolidation","home_improvement","major_purchase"))
#We will observe the new plot with these changes.
ggplot(Loan_Data, aes(x=factor(purpose)))+
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Purpose ", y = "Percentage", x= "Loan Purpose")+ theme(axis.text.x = x_axis_text_style)

#We will continue with Bivariant analysis
#We see that year does not matter for analysis so we will ignore it.
#We will mainly concentrate on the charge off parameter for our analysis

#Applications distribution loan amount wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$bin_funded_amnt))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Loan Amount / Status", x = "Amount Range", y= "Applications")+
  facet_wrap(~loan_status)

#Applications distribution Purpose wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$purpose))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Purpose / Status", x = "Purpose", y= "Applications")+
  facet_wrap(~loan_status)


#Applications distribution Interest rate wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$bin_int_rate))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Int Rate / Status", x = "Interest Range", y= "Applications")+
  facet_wrap(~loan_status)

#Applications distribution Term wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$term))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Term / Status", x = "Term", y= "Applications")+
  facet_wrap(~loan_status)

#Applications distribution Experience wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$bin_emp_length))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Experience/ Status", x = "Experience Range", y= "Applications")+
  facet_wrap(~loan_status)

#Applications distribution Verification wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$verification_status))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Verification / Status", x = "Verifications", y= "Applications")+
  facet_wrap(~loan_status)

#Applications distribution Homeownership wise status
ggplot(Loan_Data,  aes(x = as.factor(Loan_Data$home_ownership))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "violet") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)+labs(title = "Applications - Homeownership / Status", x = "HomeOwnership", y= "Applications")+
  facet_wrap(~loan_status)

