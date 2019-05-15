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


#Query 1: How many people killed were pedestrians?

fars_person_peds <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5") %>%
  group_by(st_case_year) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))

View(fars_person_peds)


#Answer: 951 pedestrians were killed. 


glimpse(fars_person)

#Query 2: How many people killed were pedestrians and minors?

fars_person_minors <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18) %>%
  group_by(AGE, st_case_year) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))

View(fars_person_minors)

#63 pedestrians killed were minors


fars_doa <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, DOA == "7") %>%
  group_by(AGE, st_case_year, DOA) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))

View(fars_doa)


#Of those 63, seven died on the scene. 

#Query 3: Of the above, how many were boys and how many were girls?

fars_person_male <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, SEX =="1") %>%
  group_by(AGE, st_case_year, SEX) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_person_male)

fars_person_female <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, SEX =="2") %>%
  group_by(AGE, st_case_year, SEX) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_person_female)

#This query shows that no values for "sex" were reported as "8" (not reported) or "9" (unknown)
fars_person_sex <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, SEX =="8") %>%
  group_by(AGE, st_case_year, SEX) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_person_sex)


#33 killed were boy and 30 killed were girls. 


fars_month <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18) %>%
  group_by(DEATH_MO) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_month)

# 11 pedestrians died in the month of June, 10 died in April, 8 died in July, 8 died December, 6 died in May, etc. 

#Query 4: How many pedestrians were involved in a car accident?

fars_peds_accidents <- fars_accident %>%
  filter(PEDS > 0) %>%
  group_by(st_case_year, PEDS) %>%
  summarise(number_records = n()) %>%
  arrange(desc(st_case_year))

View(fars_peds_accidents)


fars_sch_bus <- fars_accident %>%
  filter(FATALS > 0, SCH_BUS == "1")

View(fars_sch_bus)

#There were 1,043 reported accidents involving pedestrians. 

#Query 5: How many pedestrians under 18 were involved in a car accident in the state of Maryland?

fars_person_maryland <- fars_person %>%
  filter(PER_TYP == "5", AGE < 18, STATE =="24") %>%
  group_by(st_case_year, AGE, STATE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(AGE))
View(fars_person_maryland)

#There were 57 incidents that resulted in pedestrian minors being struck and killed by vehicles

#Query 6: In what year was the highest fatality rate for pedestrians under 18?

fars_year <- peds_u_18_accidents %>%
  group_by(YEAR) %>%
  summarise(number_records = n()) %>%
  arrange(desc(YEAR))
View(fars_year)

#Total minor fatalities in each year
  #2016: 7
  #2015: 4
  #2014: 5
  #2013: 9
  #2012: 9
  #2011: 16
  #2010: 12 

# 2011 had the highest number of fatalities involving pedestrians under the age of 18. 

#Query 7: What age group under 18 was most likely to be killed as a pedestrian?

fars_age <- pedestrian_u_18_case_numbers %>%
  group_by(AGE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_age)

#There were 13 fatalities of 15-year-olds, nine fatalities of 16-year-olds, eight fatalities of 17-year olds, seven fatalities of 14-year olds. The top four results are teenagers, therefore older teens made up the largest portion of pedestrian fatalities of minors. 

#Query 8: What was the racial breakdown of pedestrian minors killed?

fars_race <- pedestrian_u_18_case_numbers %>%
  group_by(RACE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_race)

#Race codes
  #1: White
  #2: Black
  #99: Unkown
  #98: All other races 

# 24 white children pedestrians were killed, 19 black children pedestrians were killed, and 19 children of other races were killed, while 1 child was of an unkown race. 

#Query 9: Were drugs involved in any of the accidents killing pedestrian minors?

fars_person_drug <- pedestrian_u_18_case_numbers %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18, DRUGS == "1") %>%
  group_by(st_case_year) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
view(fars_person_drug)

#There was one case in which a minor pedestrian was struck and killed.

#Query 10: What car make was most involved in accidents killing pedestrians under 18? 

fars_car_make <- peds_u_18_vehicles %>%
  group_by(MAKE) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
View(fars_car_make)

# 12 incidents that resulted in pedestrian minor deaths involved Ford cars; 8 involved Chevrolet cars, and 7 involved Toyota cars.

#Additional Queries 


fars_per_acc_join <- fars_person %>%
  inner_join(fars_accident, by=c("st_case_year" = "st_case_year"))
View(fars_per_acc_join)

fars_join_left <- fars_person %>%
  left_join(fars_vehicle, by=c("st_case_year" = "st_case_year"))
View(fars_join_left)

#New table from fars_person

fars_pedestrians <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18) %>%
  select(st_case_year, STATE, HARM_EV, MAKE, MAK_MOD, AGE, SEX, PER_TYP, INJ_SEV, RACE) %>%
  arrange(desc(st_case_year))
View(fars_pedestrians)

#New tables and joins



#62 minors peds killed

pedestrian_u_18_case_numbers <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5", AGE < 18) %>%
  distinct(st_case_year)
View(pedestrian_u_18_case_numbers)

#

pedestrian_total_case_numbers <- fars_person %>%
  filter(INJ_SEV =="4", PER_TYP == "5") %>%
  distinct(st_case_year)
View(pedestrian_total_case_numbers)

#936 total peds killsed. 6.6% of pedestrians killed between 2010 and 2016 were minors. 



peds_u_18_accidents <- fars_accident %>%
  inner_join(pedestrian_u_18_case_numbers, by ="st_case_year")
View(peds_u_18_accidents)

peds_u_18_vehicles <- peds_u_18_accidents  %>%
  inner_join(fars_vehicle, by ="st_case_year")
View(peds_u_18_vehicles)

View(pedestrian_u_18_case_numbers)
View(fars_pedestrians)

#New table from fars_accident

fars_new_accident <- fars_accident %>%
  select(st_case_year, STATE, YEAR, PEDS, HARM_EV, MAN_COLL, FATALS, DRUNK_DR) %>%
  arrange(desc(st_case_year))
View(fars_new_accident)

#Join new accidents and pedestrian tables

fars_inner_join <- fars_person %>%
  left_join(fars_accident, by=c("st_case_year" = "st_case_year"))
View(fars_inner_join)

#Total number of pedestrians killed 

fars_total_peds_death <- fars_accident
filter(PEDS > 0) %>%
  group_by(st_case_year) %>%
  summarise(number_records = n()) %>%
  arrange(desc(number_records))
