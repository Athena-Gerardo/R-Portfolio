## Covid-19 Data Cleaning & Exploration


## Remove all variables stored previously

rm(list=ls())


##Load the data into the Environment and assign it an alias

install.packages("readr")
library(readr)
CVD <- read.csv("C:/Users/fabth/Downloads/COVID19_line_list_data.csv")


## Load the packages we will be using to clean the data

install.packages("tidyverse")
library(tidyverse)

install.packages("skimr")
library(skimr)

install.packages("janitor")
library(janitor)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("anytime")
library(anytime)

install.packages("magrittr")
library(magrittr)

install.packages("Hmisc")
library(Hmisc)

library(stringr)


## View the data and any missing or incorrect observations

View(CVD)
describe(CVD)


## Clean the country column and remove NA's to make observations

CVD$country <- as.character(CVD$country)
CVD$country <- na.omit(CVD$country)
countries <- unique(CVD$country)
print(countries)

## Clean the death column to make it consistent values

CVD$death_cleaned <- as.integer(CVD$death !=0)
unique(CVD$death_cleaned) ## check if it works, and it did!


## Clean the recovered column

CVD$recovered_cleaned <- as.integer(CVD$recovered !=0)
unique(CVD$recovered_cleaned) ## check if it works, and it did!


## Clean the age column so that all ages are whole numbers

CVD$age_cleaned <- floor(CVD$age)


## Capitalize the gender column

CVD$gender_cleaned <- str_to_title(CVD$gender)
View(CVD$gender_cleaned)


## Change the formats of the columns with dates, as some are mm-dd-yy and others are mm-dd-yyy
## The dates will now be in yyy-mm-dd format
CVD_dates <- CVD %>%
  mutate(
    reporting.date_converted = anydate(reporting.date),
    symptom_onset_converted = anydate(symptom_onset),
    hosp_visit_date_converted = anydate(hosp_visit_date),
    exposure_start_converted = anydate(exposure_start),
    exposure_end_converted = anydate(exposure_end)
  )
View(CVD_dates) ## it worked, so now we can permanently change the format of our data frame CVD
## Do this for the rest of the dates too:

CVD <- CVD %>%
  mutate(
    reporting.date_converted = anydate(reporting.date),
    symptom_onset_converted = anydate(symptom_onset),
    hosp_visit_date_converted = anydate(hosp_visit_date),
    exposure_start_converted = anydate(exposure_start),
    exposure_end_converted = anydate(exposure_end)
  )
describe(CVD)
View(CVD)


## Look at the USA only

str(CVD$country)
unique(CVD$from.Wuhan)
unique(CVD$visiting.Wuhan)

CVD_USA <- CVD %>% 
  filter(country =="USA") %>% 
  group_by(location) %>% 
  summarise(
    deaths = sum(death_cleaned),
    total_visiting_Wuhan = sum(visiting.Wuhan),
    total_from_Wuhan = sum(from.Wuhan, na.rm = TRUE)
) %>% arrange(location)
View(CVD_USA)
## there does not appear to be any deaths reported within the month of Feb 2020


## Create a calculation of the death rate

(sum(CVD$death_cleaned)/nrow(CVD))*100


## Find the average age of death from Covid-19
## Create new data frames for "dead" and "alive", and use this for further calculations

dead = subset(CVD, death_cleaned == 1)
alive = subset(CVD, death_cleaned == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE) ## use na.rm to ignore the NA's for the calculations


## Create a test to ensure accuracy of the previous calculations

t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99) 
##the confidence level tells us the interval between the two variables (avg age of death vs avg age of alive)
##this is statistically significant since p_value < 0.05


## Find average death of the genders reported

men = subset(CVD, gender_cleaned == "Male")
women = subset(CVD, gender_cleaned == "Female")
mean(men$death_cleaned, na.rm = TRUE)
mean(women$death_cleaned, na.rm = TRUE)

t.test(men$death_cleaned, women$death_cleaned, alternative="two.sided", conf.level = 0.99) 
##confidence rate tells us that men have from 0.8% to 8.8% higher chance of fatality
##this is statistically significant since p_value = 0.002 < 0.05


## Find the top 5 countries with the most deaths

str(CVD$death_cleaned)
deaths_by_country <- aggregate(death_cleaned ~ country, data = CVD, sum)
deaths_by_country <- arrange(deaths_by_country, desc(death_cleaned))
print(head(deaths_by_country), 5)


## You can also find the top 5 countries with the most deaths using pipes:

total_deaths_by_country <- CVD %>%
  group_by(country) %>%
  summarise(total_deaths = sum(death_cleaned, na.rm = TRUE)) %>%
  arrange(-total_deaths) %>%
  head(5)

View(total_deaths_by_country)



