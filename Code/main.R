library(readr)
library(ggmap)
library(rworldmap)
library(plyr)
library(dplyr)
library(outliers)
setwd("../Desktop/DM/Project/")

rm(list=ls())
source("./Code/readData.R")

##########################
#### Data Preparation ####
##########################

terrorism_location = "./Data/globalterrorismdb_0718dist.csv"
military_budget_location = "./Data/MilitaryBudget.csv"
density_location = "./Data/density.csv"
emigrants_location = "./Data/refuges.csv"
population_location = "./Data/population.csv"

# Read the 5 datasets
data = readData(terrorism_location, military_budget_location, density_location, emigrants_location, population_location)
terrorism = data[[1]]
military_budget = data[[2]]
density = data[[3]]
emigrants = data[[4]]
population = data[[5]]

# Merge the 5 datasets
merged_data = subset(getMergedDataset(terrorism, military_budget, density, emigrants, 0), select = -c(year, country, attacks))

# Normalize the data
merged_data$military_budget = normalize(merged_data$military_budget)
merged_data$population_density = normalize(merged_data$population_density)
merged_data$emigrants = normalize(merged_data$emigrants)

#merged_data = rm.outlier(merged_data, fill = TRUE, median = FALSE, opposite = FALSE)

##########################
### Data Understanding ###
##########################

#Analyze the shape of the dataset.
str(terrorism)
summary(terrorism)
# The data set is very large, having about 135 variables. Some of them are very important,
# others are just repeated variables, while some of them have a lot of missing values.

# Count the number of missing values per column.
na_count <-function (x) sapply(x, function(y) sum(is.na(y)))
barplot(na_count(terrorism))
# Although the variables regarding the ransoms are interesting, they can't be used because there
#is a high number of missing entries. (~104310, 180341, 181128)

#In order to better work with the dataset, I select only the variables that seem important at a first glance and rename them.
explore_dataset = subset(terrorism, select = c(iyear, imonth, iday, country_txt, region_txt, city, latitude, longitude, summary, multiple,
                                  attacktype1_txt, targtype1_txt, targsubtype1_txt, gname, weaptype1_txt, nkill, nwound, nkillter))

explore_dataset = rename(explore_dataset, year = iyear, month = imonth, day = iday, country = country_txt, region = region_txt, multiple_attack = multiple, attacktype = attacktype1_txt, target_type = targtype1_txt, target_sub_type = targsubtype1_txt, group_name = gname, weapon_type = weaptype1_txt)

#Attacks over time
r = explore_dataset %>%
  group_by(year) %>%
  summarise( nr_of_attacks = n()) %>%
  
plot(r$year, r$nr_of_attacks, type='l')
#The trend of attacks seems to be ascending.

#Attack type.
explore_dataset %>%
  group_by(attacktype) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)
# Most of the attacks are bombing attacks -> 80.000. Second place is Armed Assault cca 40.000.

data_after_2000 = subset(explore_dataset, explore_dataset$year > 2010)

world <- borders("world", colour="gray50", fill="gray50") 
worldmap <- ggplot() + world + scale_y_continuous(limits=c(-55, 90))

worldmap + 
  geom_point(aes(x=data_after_2000$longitude[explore_dataset$nkill<51], y=data_after_2000$latitude[explore_dataset$nkill<51]), col='blue', alpha= 0.2) +
  geom_point(aes(x=data_after_2000$longitude[explore_dataset$nkill>50], y=data_after_2000$latitude[explore_dataset$nkill>50]), col='red', size=2) +
  labs(title='Location of terrorist attacks by severity')

explore_dataset %>%
  group_by(region) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)
#By far the most dangerous area is the Middle East and north Africa. Then, we have South Asia and South America.
#Europe tends to be a safer place, having aproximately a thirds of the number of attacks from Middle East.

explore_dataset %>%
  group_by(country) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)


#Perhaps not surprisingly, the most dangerous country in terms of terrorism is Iraq. As we will see later, it is the homeland
# of dangerous terrorist groups like ISIL. Then, we have its neighbours, Pakistan and Afghanistan.
# The most targetted country from EU is UK, followed by Turkey.

explore_dataset %>%
  filter(city != 'Unknown') %>%
  group_by(city) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)
#The most targetted city is Baghdad.


################
#### Models ####
################

### Classification ####

source('./Code/classification.R')

### Regression ####
source('./Code/regression.R')


### Association Rules Mining ####
source('./Code/association_rules.R')

### Clustering ###
source('./Code/clustering.R')
