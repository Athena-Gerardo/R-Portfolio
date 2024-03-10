## Star Wars Data Cleaning


## Remove all variables stored previously

rm(list=ls())


##Load the data into the Environment and explore the observations

install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(tidyr)
data(starwars)
sw <- starwars
glimpse(sw)
View(sw)


# Change the gender variables from chr to factor variables

class(sw$gender)
unique(sw$gender)

sw$gender <- as.factor(sw$gender)
class(sw$gender)


## Change the order of the factor levels (useful for small, med, large options, etc)

levels(sw$gender) 
sw$gender <- factor((sw$gender),
                     levels = c("masculine", 
                                "feminine"))
levels(sw$gender)


## Look at the variables

sw %>% 
  select(name, height, ends_with("color")) %>%
  names() #gives just the column names

unique(sw$hair_color)

sw %>% 
  select(name, height, ends_with("color")) %>%
  filter(hair_color %in% c("blond", "brown") &
           height < 180)


## Address the missing data

mean(sw$height, na.rm = TRUE)
# na.omit() doens't show what has been removed, it's best to avoid

sw %>%
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.)) # this helps show what specific values are missing, best to use

# we can change the options for some rows with missing data, and drop the others where na can't be changed

#remove the observations where height is na
sw %>%
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.)) %>%
  drop_na(height)

#replace na in the hair color column
sw3 <- sw %>%
      mutate(hair_color = replace_na(hair_color, "None"))
View(sw3)
 sw <- sw %>%
   mutate(hair_color = replace_na(hair_color, "None"))
 View(sw)
 
 
## Address duplicates

sw[!duplicated(sw),] 
!duplicated(sw) # no duplicates exist

sw %>% 
  distinct() # the tibble is 87 x 14, this tells us all values in the data frame are distinct
             # and no duplicates exist


## Recode variables for further analysis, create new table for this as I don't want to permanently change sw data frame

sw1 <- sw %>%
       select(name, gender) %>%
       mutate(gender = recode(gender,
                              "masculine" = 1,
                              "feminine" = 2))
View(sw1)


## Clean the vehicles and the starships columns

sw3$vehicles[sw3$vehicles == "character(0)"] <- "Unknown"
sw3$starships[sw3$starships == "character(0)"] <- "Unknown"

#now permanently change the sw data frame
sw$vehicles[sw$vehicles == "Character(0)"] <- "Unknown"
sw$starships[sw$starships == "Character(0)"] <- "Unknown"
View(sw)


## Capitalize all column observations (I always test on another variable before making permanent changes)

sw2 <- sw %>%
       select(hair_color, skin_color, eye_color, gender, sex) %>%
       mutate(across(where(is.character),str_to_title)) %>%
       mutate(across(where(is.factor), str_to_title))
View(sw2)

sw <- sw %>%
  mutate(across(where(is.character),str_to_title)) %>%
  mutate(across(where(is.factor), str_to_title)) %>%
  mutate(across(where(is.list), str_to_title))
View(sw)


## Separate the hair_color column where hair_color is more than one, and replace all na's with "None"

swtest <- sw %>% 
          separate(hair_color, c('hair_color', 'hair_color2')) %>%
          mutate(hair_color2 = replace_na(hair_color2, "None"))
View(swtest)

#permanently change the sw data frame
sw <- sw %>% 
  separate(hair_color, c('hair_color', 'hair_color2')) %>%
  mutate(hair_color2 = replace_na(hair_color2, "None"))
View(sw)


## Separate the skin_color column where skin_color is more than one, and replace all na's with "None"

swtest <- sw %>% 
  separate(skin_color, c('skin_color', 'skin_color2', 'skin_color3')) %>%
  mutate(skin_color2 = replace_na(skin_color2, "None"),
         skin_color3 = replace_na(skin_color3, "None"))
View(swtest)

#permanently change the sw data frame
sw <- sw %>% 
  separate(skin_color, c('skin_color', 'skin_color2', 'skin_color3')) %>%
  mutate(skin_color2 = replace_na(skin_color2, "None"),
         skin_color3 = replace_na(skin_color3, "None"))
View(sw)













