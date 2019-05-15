#New R script 

#Install tidyverse packages

install.packages("tidyverse")
install.packages('backports')
install.packages('broom')
install.packages('lubridate')
install.packages("dplyr") 

#Load packages

library("tidyverse")
library('lubridate')

#Import datasets

fars_accident <- read_csv("fars.accident.csv")
View(fars_accident)

fars_person <- read_csv("fars.person.csv")
View(fars_person)

fars_vehicle <- read_csv("fars.vehicle.csv")
View(fars_vehicle)

fars_distract <- read_csv("fars.distract.csv")
View(fars_distract)

fars_pbtype <- read_csv("fars.pbtype.csv")
View(fars_pbtype)


#Question 1: How many pedestrians were involved in traffic accidents? PERSON TABLE

fars_total_peds <- fars_person %>%
  filter(PER_TYP == "5") %>%
  group_by(INJ_SEV) %>%
  summarise(number_records = n())  %>%
  arrange(INJ_SEV)
View(fars_total_peds)

# 1028 pedestrians were involved in crashes between 2010-2016. 

#Question 2: How many pedestrians were killed in traffic accidents? PERSON TABLE

fars_total_peds_per <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_per)

barplot(fars_total_peds_per)

ggplot(data=fars_total_peds_per, aes(x=DEATH_YR, y=number_records)) +
  geom_bar(stat="identity")

# 932 pedestrian killed in traffic accidents between 2010-2016. 

#2016: 137
#2015: 136
#2014: 132
#2013: 138
#2012: 125
#2011: 128
#2010: 136


# 0.7% increase from 2010 to 2016 in deaths involving pedestrians. 
# PCT change 2015-2016: 0.7% increase
# PCT change 2014-2015: 3% increase
# PCT change 2013-2014: 4.3% decrease
# PCT change 2012-2013: 10.4% increase
# PCT change 2011-2012: 2.3% decrease
# PCT change 2010-2011: 5.9% decrease



#Question 3: How many pedestrian deaths took place in Maryland?

fars_total_peds_md <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_md)

#688 total pedestrian deaths in Maryland from 2010-2016. 

#Question 4: How many pedestrian deaths took place in Maryland just in 2016?

fars_total_peds_md_2016 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR =="2016") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_md_2016)

#102 pedestrian deaths in Maryland in 2016. 

#Question 5: How many pedestrian deaths took place in Delaware?

fars_total_peds_de <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="10") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_de)

#177 pedestrian deaths in Deleware between 2010-2016. 


#Question 6: How many pedestrian deaths took place in Delaware just in 2016?

fars_total_peds_de_2016 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="10", DEATH_YR == "2016") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_de_2016)

#27 deaths in DE in 2016. 

#Question 7: How many pedestrian deaths took place in DC?

fars_total_peds_dc <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="11") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_dc)

#67 pedestrian deaths in DC between 2010-2016.

#Question 8: How many pedestrian deaths took place in DC just in 2016?

fars_total_peds_dc_2016 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="11", DEATH_YR == "2016") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_peds_dc_2016)

#8 deaths in DC in 2016. 

#Question 9: Looking at pedestrian deaths in Maryland between 2010-2016 by age. 

fars_peds_age_md <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR != 9999, DEATH_YR != 2017, AGE != "998", AGE != "999") %>%
  group_by(AGE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_peds_age_md)

#Question 10: Looking at pedestrian minor deaths in Maryland between 2010-2016 by age.

fars_peds_age_md_minor <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR != 9999, DEATH_YR != 2017, AGE < 18) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_peds_age_md_minor)

#Question 11: Looking at pedestrian deaths in 2016 alone by age.

fars_peds_2016_md <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR == "2016", AGE != "998") %>%
  group_by(AGE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_peds_2016_md)

#Age ranges

# 0-17: 4 records
# 18-29: 28 records 
# 30-39: 8 records 
# 40-49: 14 records
# 50-59: 20 records
# 60-69: 17 records
# 70-79: 6 records
# 80-89: 3 records
# 90-99: 1 record 

#Oldest person to die was 90 and youngest person to die was 5 in 2016 in MD

#Question 12: Learning more about the 90-year-old pedestrian who died.

fars_peds_90 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR == "2016", AGE != "998", AGE > 89)
View(fars_peds_90)

# Occured on August 14, 2016 at 17:54 p.m. not At Intersection-On Roadway, Not in Marked Crosswalk The pedestrian was a white, non-Hispanic 90-year-old woman. Alcohol and drugs were not involved. The victim was transported to the hospital by EMS Air. She died 10 days later on August 24, 2016 at 03:42 a.m.

#Question 13: Learning more about the 5-year-old pedestrian who died

fars_peds_5 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR == "2016", AGE != "998", AGE < 6)
View(fars_peds_5)

vehicles_5 <- fars_vehicle %>%
  filter(st_case_year == "240050_2016")
View(vehicles_5)

accident_5 <- fars_accident %>%
  filter(st_case_year == "240050_2016")
View(accident_5)

distract_5 <- fars_distract %>%
  filter(st_case_year == "240050_2016")
View(distract_5)

# At 10:20 a.m. on Monday, Jan. 25, 2016, a 5-year-old girl was struck and killed by a Kia vehicle in Carroll County, Maryland. She died less than an hour later at 11:09 a.m. The driver, a 49-year-old woman, was not reported to be distracted, speeding or under the influence of alcohol or drugs at the time of the collision. 

#Question 14: Looking at pedestrian deaths in Delaware in 2016 alone by age.

fars_peds_2016_de <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="10", DEATH_YR == "2016", AGE != "998") %>%
  group_by(AGE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_peds_2016_de)

#Age ranges

# 0-17: 3 records
# 18-29: 1 records 
# 30-39: 5 records 
# 40-49: 7 records
# 50-59: 4 records
# 60-69: 3 records
# 70-79: 2 records
# 80-89: 2 records
# 90-99: 0 records 

#Oldest person to die was 84 and youngest person to die was 14 in 2016 in DE.

#Question 15: Looking at pedestrian deaths in DC in 2016 alone by age.

fars_peds_2016_dc <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="11", DEATH_YR == "2016", AGE != "998") %>%
  group_by(AGE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_peds_2016_dc)

#Age ranges

# 0-17: 0 records
# 18-29: 2 records 
# 30-39: 2 records 
# 40-49: 1 records
# 50-59: 1 records
# 60-69: 1 records
# 70-79: 1 records
# 80-89: 0 records
# 90-99: 0 records 

#Oldest person to die was 77 and youngest person to die was 23 in 2016 in DC

#Question 16: Were the drivers who struck and killed pedestrians in Maryland in 2016 under the influence of drugs?

fars_peds_drug<- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR == "2016", DRUGS == "0") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_peds_drug)

# Only 3 incidents with reported drug use in which a pedestrian was struck and killed.  

#Question 17: Where were fatal pedestrians accidents in Maryland in 2016 most likely to occur?

fars_peds_location_2016 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", DEATH_YR == "2016", AGE != "998") %>%
  group_by(LOCATION) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_peds_location_2016)

ggplot(data=fars_peds_location_2016, aes(x=LOCATION, y=number_records)) +
  geom_bar(stat="identity")

# Most fatal pedestrian accidents in 2016 occured in "Non At Intersection-On Roadway, Not in Marked Crosswalk"

#Question 18: Where were fatal pedestrians accidents in Maryland between 2010-2016 most likely to occur?

fars_peds_location <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE =="24", AGE != "998", LOCATION != "99", LOCATION != "98", LOCATION != "28") %>%
  group_by(LOCATION) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_peds_location)

ggplot(data=fars_peds_location, aes(x=LOCATION, y=number_records)) +
  geom_bar(stat="identity")

# Most fatal pedestrian accidents between 2010-2016 occured in "Non At Intersection-On Roadway, Not in Marked Crosswalk"

#Question 19: How do fatal pedestrians accidents compare to fatal bicyclist accidents?

fars_bike <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "6") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_bike)

fars_peds <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_peds)

# 75 bicyclist deaths between 2010-2016 compared to 932 pedestrian deaths in the same time frame. 

#Question 20: How do fatal pedestrians accidents compare to fatal accidents in which the driver died?

fars_drivers <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "1") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_drivers)

# 2,536 driver deaths between 2010-2016 compared to 932 pedestrian deaths in the same time frame. 

#Question 21: How do fatal pedestrians accidents compare to fatal accidents in which a passenger died?

fars_pass <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "2") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_pass)

# 709 passanger deaths between 2010-2016 compared to 932 pedestrian deaths in the same time frame. 

#Question 22: How many fatal traffic accidents occurred between 2010 and 2016?

fars_total_deaths <- fars_person %>%
  filter(INJ_SEV =="4") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR)) %>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_total_deaths)

ggplot(data=fars_total_deaths, aes(x=DEATH_YR, y=number_records)) +
  geom_bar(stat="identity")

# 4, 282 traffic deaths 

# 2016: 637
# 2015: 658
# 2014: 574
# 2013: 566
# 2012: 617
# 2011: 612
# 2010: 618

#Question 23: How many fatal accidents involved school buses?

fars_school_bus <- fars_accident %>%
  filter(SCH_BUS == "1", FATALS > 0) %>%
  group_by(YEAR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(YEAR)) %>%
  filter(YEAR != 9999, YEAR != 2017)
View(fars_school_bus)

ggplot(data=fars_school_bus, aes(x=YEAR, y=number_records)) +
  geom_bar(stat="identity")

# 14 incidents involving school buses resulted in deaths. 

#Question 24: How many fatal accidents involving pedestrian minors in all states between 2010 and 2016?

fars_person_minors <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, DEATH_YR != 9999) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))

View(fars_person_minors)

ggplot(data=fars_person_minors, aes(x=DEATH_YR, y=number_records)) +
  geom_bar(stat="identity")

#61 fatal accidents involving pedestrian minors in all states between 2010 and 2016.

#Question 25: In how many fatal accidents did pedestrian minors die on the scene?

fars_doa <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, DOA == "7") %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))

View(fars_doa)

# There are only records 7 records for they years 2010, 2011, and 2012 for accidents involving minor and pedestrians in which an person died on the scene.

#Question 26: Accidents involving dead pedestrian minors broken down by gender

fars_person_male <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, SEX =="1", DEATH_YR != 9999) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_person_male)

fars_person_female <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, SEX =="2", DEATH_YR != 9999) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_person_female)

#31 accidents in which boys were killed and 30 accidents in which girls were killed.

#Question 27: Accidents involving all dead pedestrians broken down by gender

fars_person_male_all <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", SEX =="1", STATE == "24", DEATH_YR != 9999, DEATH_YR != 2017) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_person_male_all)

fars_person_female_all <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", SEX =="2", STATE == "24", DEATH_YR != 9999, DEATH_YR != 2017) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_person_female_all)

# 495 accidents in which men were killed and 192 accidents involving girls.

#Question 28: How many deadly pedestrian minor accidents occurred in Maryland?

fars_person_maryland <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, STATE =="24", DEATH_YR != 9999) %>%
  group_by(DEATH_YR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(DEATH_YR))
View(fars_person_maryland)

# There was a total of 44 fatal accidents in Maryland invovling pedestrians and minors. 

#Question 29: What was the make of car most involved in deadly traffic accidents with pedestrian under the age of 18?

fars_car_make <- peds_u_18_vehicles %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, MAKE != 99) %>%
  group_by(MAKE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_car_make)

#Fords were most involved in fatal traffic accidents with pedestrian under the age of 18.

#Question 30: What was the make of car most involved in deadly pedestrian traffic accidents?

fars_car_make_all <- peds_u_18_vehicles %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", MAKE != 99) %>%
  group_by(MAKE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(MAKE))%>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_car_make_all)

#Fords were involved in 12 accidents, Chevrolet involved in 8 acccidents, Toyotas in 7 accidents (out of 69)


#TABLE JOINS: Joining FARS ACCIDENTS table to FARS PERSON table of deaths, pedestrians and minors 

peds_deaths <- fars_vehicle %>%  
  inner_join(fars_person, by = "st_case_year") %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", DEATH_YR != 9999, DEATH_YR != 2017)
View(peds_deaths)

#Question 31: Between 2010-2016, what was the make of car most involved in deadly pedestrian traffic accidents?

new_peds_deaths <- peds_deaths %>%
  group_by(MAKE.y) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(new_peds_deaths)

#TOP 5 (excluding unknown make)

#1 - Ford - 115
#2 - Chevrolet - 77
#3 - Toyota - 68
#4 - Honda - 60 
#5 - Datsun/Nissan - 33

#Question 32: In 2016, what was the make of car most involved in deadly pedestrian traffic accidents?

peds_deaths_2016 <- fars_person %>%  
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", DEATH_YR == "2016")%>%
  inner_join(fars_vehicle, by = "st_case_year")
View(peds_deaths_2016)

new_peds_deaths_2016 <- peds_deaths_2016 %>%
  group_by(MAKE.y) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(new_peds_deaths_2016)

#TOP 5 for 2016 (excluding unknown make)

#1 - Chevrolet - 14
#2 - Ford - 13
#3 - Honda - 11
#4 - Toyota - 11  
#5 - Dodge - 7


#More Table joins 

View(fars_vehicle)

vehicles_persons_accident <- fars_person %>%
  inner_join(fars_accident, by = "st_case_year") %>%
  inner_join(fars_vehicle, by = "st_case_year")
View(vehicles_persons_accident)

new_vehicles_persons_accident <- vehicles_persons_accident %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", DEATH_YR != 9999, DEATH_YR != 2017) %>%
  group_by(MAKE.x)
summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(new_vehicles_persons_accident)

peds_deaths_all <- fars_vehicle %>%
  inner_join(peds_deaths, by = "st_case_year")
View(peds_deaths_all)

fars_car_make_all <- peds_deaths_all %>%
  group_by(MAKE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(MAKE))%>%
  filter(DEATH_YR != 9999, DEATH_YR != 2017)
View(fars_car_make_all)

pedestrian_u_18_case_numbers <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18) %>%
  distinct(st_case_year)
View(pedestrian_u_18_case_numbers)

peds_u_18_accidents <- fars_accident %>%
  inner_join(pedestrian_u_18_case_numbers, by ="st_case_year")
View(peds_u_18_accidents)

peds_u_18_vehicles <- peds_u_18_accidents  %>%
  inner_join(fars_vehicle, by ="st_case_year")
View(peds_u_18_vehicles)

fars_vehicles_make <- fars_vehicle  %>%
  inner_join(fars_accident, by = "st_case_year")%>%
  inner_join(fars_person, by = "st_case_year")
view(fars_vehicles_make)

peds_vehicle_make <- fars_vehicles_make  %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", MAKE.x != 99, YEAR ) %>%
  group_by(MAKE.x) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(peds_vehicle_make)

pedestrians_killed_2016 <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", DEATH_YR == "2016") %>%
  distinct(st_case_year)
View(pedestrians_killed_2016)

pedestrians_killed_all_years <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", STATE == "24", DEATH_YR != 9999) %>%
  distinct(st_case_year)
View(pedestrians_killed_all_years)

#New table with info from fars_person, fars_accident, and fars_distract

new_table_2016 <- fars_distract %>%
  inner_join(pedestrians_killed_2016, by ="st_case_year") %>%
  inner_join(fars_accident, by ="st_case_year")
View(new_table_2016)

#Question 33: How many deadly pedestrian traffic accidents were caused by distracted driving?

deaths_by_distraction_2016 <- new_table_2016 %>%
  group_by(MDRDSTRD) %>%
  summarise(number_records = n()) %>%
  arrange(desc(MDRDSTRD)) 
View(deaths_by_distraction_2016)

new_table_all_years <- fars_distract %>%
  inner_join(pedestrians_killed_all_years, by ="st_case_year") %>%
  inner_join(fars_accident, by ="st_case_year")
View(new_table_all_years)

deaths_by_distraction <- new_table_all_years %>%
  group_by(MDRDSTRD) %>%
  summarise(number_records = n()) %>%
  arrange(desc(MDRDSTRD)) 
View(deaths_by_distraction)

#75/113 accidents was not caused by distracted driving. 

#Question 34: What were weather conditions like at the time of deadly pedestrian traffic accidents?

join_person_accident <- pedestrians_killed_all_years %>%
  inner_join(fars_accident, by ="st_case_year")
View(join_person_accident)

weather_conditions <- join_person_accident %>%
  group_by(WEATHER) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records)) 
View(weather_conditions)

#A majority of accidents occurred in clear weather 

#Question 35: What were weather conditions like at the time of deadly pedestrian traffic accidents in Maryland in 2016?

join_person_accident_2016 <- pedestrians_killed_2016 %>%
  inner_join(fars_accident, by ="st_case_year")
View(join_person_accident_2016)

weather_conditions_2016 <- join_person_accident_2016 %>%
  group_by(WEATHER) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records)) 
View(weather_conditions_2016)

# Again, a majority of accidents occurred in clear weather in 2016 in Maryland. 

#Question 36: Where were fatal pedestrians accidents in Maryland between 2010-2016 most likely to occur? Location filtered by RELJCT2 in accidents table all years

relation_junction <- join_person_accident %>%
  group_by(RELJCT2) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records)) 
View(relation_junction)

#Most occured in "non-junctions"

#Question 37: Where were fatal pedestrians accidents in Maryland in 2016 most likely to occur? Location filtered by RELJCT2 in accidents table 2016

relation_junction_2016 <- join_person_accident_2016 %>%
  group_by(RELJCT2) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records)) 
View(relation_junction_2016)

#Most occured in "non-junctions"

#Question 37: In what counties were fatal pedestrians accidents in Maryland between 2010-2016 most likely to occur? 

county_all_years <- join_person_accident %>%
  group_by(COUNTY) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records)) 
View(county_all_years)

#COUNTY (ALL YEARS) 

# 33 = Prince George's County - 147
# 5 = Baltimore - 128
# 510 = Baltimore (City) - 75
# 31 = Montgomery - 69
# 3 = ANNE ARUNDEL - 60
# 25 = Harford - 28
# 17 = Charles - 23
# 15 = Cecil - 21
# 43 = Washington - 21
# 37 = St. Mary's - 17
# 27 = Howard - 15
# 13 = Carroll - 14
# 47 = Worcester - 14
# 21 = Frederick - 10
# 45 = Wicomico - 7
# 23 = Garret - 5
# 35 = Queen Anne's - 5
# 19 = Dorchester - 4
# 41 = Talbot - 4
# 9 =  Calvert - 3
# 1 = Allegany - 2
# 11 = Caroline - 2
# 39 = Somerset - 2

#676 total

#Question 38: In what counties were fatal pedestrians accidents in Maryland in 2016 most likely to occur? 

county_2016 <- join_person_accident_2016 %>%
  group_by(COUNTY) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records)) 
View(county_2016)

#COUNTY (2016) 

# 5 = Baltimore - 19
# 33 = Prince George's County - 19
# 510 = Baltimore (City) - 18
# 27 = Howard - 8
# 31 = Montgomery - 8
# 3 = ANNE ARUNDEL - 7
# 13 = Carroll - 4
# 17 = Charles - 4
# 25 = Harford - 4
# 43 = Washington - 3 
# 47 = Worcester - 3
# 15 = Cecil - 2
# 37 = St. Mary's - 2
# 9 =  Calvert - 1
# 19 = Dorchester - 1
# 35 = Queen Anne's - 1

#101 total



