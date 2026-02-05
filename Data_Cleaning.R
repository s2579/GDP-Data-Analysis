library(tidyverse)
library(countrycode)

setwd("/Users/swim/Desktop/My Projectss/Data_analysis_Cleaning")

#Accessing the CSV file 
GDP_PPP <- read.csv("GDP_PPP.csv")
Rule_of_Law <- read.csv("Rule_of_law.csv")
Ranking <- read.csv("Rankings.csv")

#First task: create a Primary key 

#Ranking dataset does not have ISO-3 value, so we'll create one for her. 

Ranking$iso3 <- countrycode(Ranking$Economy,
                            origin = 'country.name',
                            destination = 'iso3c')

#Replace string 'S' with 'ARM' 

GDP_PPP$Country.Code[GDP_PPP$Country.Code == 'S'] <- 'ARM'

#Update Kosovo ISO-3
Ranking$iso3[is.na(Ranking$iso3)] <- 'XKX'


#Remove aggregrates in rule of law and GDP_PPP
cleaned_GDP_PP <- GDP_PPP %>%  
  filter(nchar(Country.Code) == 3) %>%
  filter(Country.Code %in% codelist$iso3c | Country.Code == 'XKX') %>%
  filter()

cleaned_Rule_of_Law <- Rule_of_Law %>%  
  filter(nchar(Country.Code) == 3) %>%
  filter(Country.Code %in% codelist$iso3c | Country.Code == 'XKX')

updated_GDP_PPP <- rename(cleaned_GDP_PP, iso3 = Country.Code)
updated_ruleoflaw <- rename(cleaned_Rule_of_Law, iso3 = Country.Code)

new_dataset <- merge(updated_GDP_PPP, updated_ruleoflaw, by = "iso3")
new_dataset2 <- merge(new_dataset, Ranking, by = "iso3")

#Clean new_dataset2 

#Vector with all coloumns to be removed 
remove_these <- c("Indicator.Code.x", "Indicator.Code.y", "Indicator.Name.x", 
                  "Indicator.Name.y", "Country.Name.y", "X.y","X.x" , "Economy", "X2024.y" )

#Use Index to exlcude values listed above 
new_dataset2 <- new_dataset2[, !names(new_dataset2) %in% remove_these]


years_update <- str_c("X",as.character(1990:1995))

new_dataset2 <- new_dataset2 %>% rename_with(~paste0(.,".x"), all_of(years_update))

#Remove lichestein (missing values)
new_dataset2 <- new_dataset2[-100,]

#Log GDP PPP 2021
new_dataset2 <- new_dataset2 %>% 
  mutate(across(matches("\\d{4}.x"), ~ log(.)))

#Convert the "DB 2020 Ranking" into index score 

new_dataset2 <- new_dataset2 %>% 
  mutate(indexed = 1 - (globalRank/189))

#Rule of Law transformation 
new_dataset2 <- new_dataset2 %>% 
  mutate(across(matches("\\d{4}.y"), ~./100))

#Dummy Variable for Oil Endowed Nations

oil_endowed_nations <- c(
  "DZA", "BHR", "IRN", "IRQ", "KWT", "LBY", "OMN", "QAT", 
  "SAU", "ARE", "VEN", "GUY", "MEX", "COL", 
  "RUS", "KAZ", "AZE", "NOR", "NGA", "AGO", 
  "GAB", "GNQ")

#Create a new coloumn assigning 1 to oil endowed nations and 0 to others 
new_dataset2 <- new_dataset2 %>% 
  mutate(Oil_Endowed = case_when(
    iso3 %in% oil_endowed_nations ~ 1,  
    !(iso3 %in% oil_endowed_nations) ~ 0
  ))











