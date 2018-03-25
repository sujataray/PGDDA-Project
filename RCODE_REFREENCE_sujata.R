library(dplyr)
library(tidyr)
library(stringr)
setwd("C:/Swadi/PG/Invest_Case_Study_Team")

#CP1:Data Cleaning
#Read the companies txt file with tab as seperator
companies <- read.delim("companies.txt", sep="\t")
#Convert all the letter in  permalink to lower case 
companies$permalink <- sapply(companies$permalink, tolower)
#verify the same by viewing them
View(companies)

#Read the rounds2 csv file
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
#Convert all the letter in  company_permalink to lower case for comparision 
rounds2$company_permalink <- sapply(rounds2$company_permalink, tolower)
#verify the same by viewing them
View(rounds2)

#Find unique no of companies in companies file 
count(distinct(companies, permalink))
#OR
length(unique(companies$permalink))

#Find unique no of companies in rounds2 file 
count(distinct(rounds2, company_permalink))
#OR
length(unique(rounds2$company_permalink))

#Merge the companies data frame to rounds2
master_frame <- merge(rounds2, companies, by.x=c("company_permalink"), by.y=c("permalink"))

#Check if there are any companies in round that are not in companies file
length(unique(master_frame$company_permalink))

#---------------------------------------------------

#CP2: Funding Type Analysis
#Spark Funds wants to invest in one of these 4 types (venture/Angel/seed/Private)based on 
#average funding and thier constraints (of 5-15m) 

#Understand/note average funding for each type of funding and update the xls (Table2.1)
View(summarise(group_by(master_frame, funding_round_type), Average_Funding=mean(raised_amount_usd, na.rm=T)))
#OR
View(aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN=mean, na.rm=TRUE))

#-----------------------------------------------------------------

#CP3:Country Analysis for Venture type of funding (This is based on previous analysis)
#Create new DF with only venture funded companies from combined DF
venture <- filter(master_frame, funding_round_type == "venture")

#Find the venture funded companies by total amount raised by country
topcountries <- summarise(group_by(venture, country_code), Sum_Funding=sum(raised_amount_usd, na.rm=T))

#Arrange the countries based on the funding (high to low)
toparrange <- arrange(topcountries, desc(Sum_Funding))

#Get top9 countries from this list (and select top 3 english speaking countries 
#using the reference pdf english country list and update table3.1)
top9 <- head(toparrange, 9)
View(top9)

#---------------------------------------------------------

#CP4: Sector Analysis - for top 3 countries (USA, GBR, IND)
#Read mapping file and convert wide format to long format
mapping <- read.csv("mapping.csv", stringsAsFactors = F)
#Some of the category names have "0" in place of "Na or na". So replace "0" by "na"
mapping$category_list <- str_replace_all(mapping$category_list, "0", "na")
#For category list starting with "0" in mapping file - we have replaced by "na" - since camparsion
#join is case sensitive - for the category list starting with na - we need to make first character
#capital
mapping$category_list <- str_to_title(mapping$category_list)
View(mapping)
newdata <- gather(mapping, sector_type, my_val, Automotive...Sports:Social..Finance..Analytics..Advertising)
#Enable sectors only for the applicable category list
newdata <- newdata[!(newdata$my_val == 0),]
View(newdata)

#In the master frame have only primary category in category list
master_frame <- separate(master_frame, category_list, into=c("category_list"), sep="[|]")

#Using hte newdata frame created above - map the category list of master frame
#add new sector column to the master frame at the end.
master_frame_with_sector <- merge(master_frame, newdata, by.x=c("category_list"), by.y=c("category_list"), all.x=TRUE)
View(master_frame_with_sector)
write.table(master_frame_with_sector, "Merged_Master")
#-----------------------------------------------------------------------
#CP5:Sector Analysis 2
#We have sectore in master frame and we know 3 countries for Venture funding
#varying between 5 to 15Million
#Create 3 Data frames (D1, D2, D3) for 3 countries - fill table 5.1
DF_USA <- filter(master_frame_with_sector, country_code == "USA", funding_round_type=="venture")
DF_GBR <- filter(master_frame_with_sector, country_code == "GBR", funding_round_type=="venture")
DF_IND <- filter(master_frame_with_sector, country_code == "IND", funding_round_type=="venture")

#Total number of Investments (count)
count_usa <- sum((!is.na(DF_USA$raised_amount_usd)))
View(count_usa)
count_gbr <- sum((!is.na(DF_GBR$raised_amount_usd)))
View(count_gbr)
count_ind <- sum((!is.na(DF_IND$raised_amount_usd)))
View(count_ind)

#Total amount of investment (USD)
sum(DF_USA$raised_amount_usd, na.rm=TRUE)
sum(DF_GBR$raised_amount_usd, na.rm=TRUE)
sum(DF_IND$raised_amount_usd, na.rm=TRUE)

#Top Sector name (no. of investment-wise)
#grouped the DF_USA based on sectors and summarised to get count based on the my_val
sectorwise_USA <- group_by(DF_USA,sector_type)
sectorwise_investment_USA <- summarise(sectorwise_USA, No_of_Investment = sum(my_val, na.rm = T))
View(arrange(sectorwise_investment_USA,desc(No_of_Investment)))

#grouped the DF_GBR based on sectors and summarised to get count based on the my_val
sectorwise_GBR <- group_by(DF_GBR,sector_type)
sectorwise_investment_GBR <- summarise(sectorwise_GBR, No_of_Investment = sum(my_val, na.rm = T))
View(arrange(sectorwise_investment_GBR,desc(No_of_Investment)))

#grouped the DF_IND based on sectors and summarised to get count based on the my_val
sectorwise_IND <- group_by(DF_IND,sector_type)
sectorwise_investment_IND <- summarise(sectorwise_IND, No_of_Investment = sum(my_val, na.rm = T))
View(arrange(sectorwise_investment_IND,desc(No_of_Investment)))

#For point 3 (top sector count-wise), which company received the highest investment?
#Filtered the DF_USA based on Others sector and arranged the same in desc order based on Investment
filtered_data_USA <- filter(sectorwise_USA,sector_type=="Others")
View(arrange(filtered_data_USA,desc(raised_amount_usd)))

#Filtered the DF_GBR based on Others sector and arranged the same in desc order based on Investment
filtered_data_GBR <- filter(sectorwise_GBR,sector_type=="Others")
View(arrange(filtered_data_GBR,desc(raised_amount_usd)))

#Filtered the DF_IND based on Others sector and arranged the same in desc order based on Investment
filtered_data_IND <- filter(sectorwise_IND,sector_type=="Others")
View(arrange(filtered_data_IND,desc(raised_amount_usd)))

#For point 4 (second best sector count-wise), which company received the highest investment?
#Filtered the DF_USA based on Cleantech...Semiconductors and arraned it in desc order of Investment
filtered_data2_USA <- filter(sectorwise_USA,sector_type=="Cleantech...Semiconductors")
View(arrange(filtered_data2_USA,desc(raised_amount_usd)))

#Filtered the DF_GBR based on Cleantech...Semiconductors and arraned it in desc order of Investment
filtered_data2_GBR <- filter(sectorwise_GBR,sector_type=="Cleantech...Semiconductors")
View(arrange(filtered_data2_GBR,desc(raised_amount_usd)))

#Filtered the DF_IND based on Social..Finance..Analytics..Advertising
filtered_data2_IND <- filter(sectorwise_IND,sector_type=="Social..Finance..Analytics..Advertising")
View(arrange(filtered_data2_IND,desc(raised_amount_usd)))

#Create a combined csv file for usage in Tableau
write.table(master_frame_with_sector, "Merged_Master")

