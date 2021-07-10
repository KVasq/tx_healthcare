library(dplyr)

#load csvs

insurance_rates <- read.csv("tx_county_insurance_rates.csv")
demographics <- read.csv("tx_county_summary.csv")
samples <- read.csv("data_day_sample_20180410_export.csv")
county_region <- read.csv("county_to_media_region.csv")

county_key <- demographics %>% summarise(county_name, censuskey)

#What type of people are insured and uninsured?

#remove nan and rows w/ empty values
samples <- samples[samples$health_insurance != "",] %>% na.omit()

#add dummy variable for being insured
samples$is_insured <- ifelse(samples$health_insurance == "Uninsured", 0, 1)

summary(lm(is_insured ~ income, data = samples)) # *
summary(lm(is_insured ~ ethnicity_afam, data = samples))
latino_insured <- summary(glm(is_insured ~ ethnicity_latino, data = samples, family = "binomial")) # *** -
summary(lm(is_insured ~ ethnicity_white, data = samples)) # ***
summary(lm(is_insured ~ ethnicity_asian, data = samples)) # . 

samples$level_of_education <- as.factor(samples$level_of_education)
samples$education_area <- as.factor(samples$education_area)

summary(lm(is_insured ~ level_of_education, data = samples)) # *** -
summary(lm(is_insured ~ education_area, data = samples))
summary(lm(is_insured ~ age, data = samples)) # ***

summary(lm(is_insured ~ occupation_blue_collar, data = samples))
summary(lm(is_insured ~ occupation_farmer, data = samples))
summary(lm(is_insured ~ occupation_professional_technical, data = samples)) # ***
summary(lm(is_insured ~ occupation_retired, data = samples))
summary(lm(is_insured ~ length_of_residence, data = samples)) # .

summary(lm(is_insured ~ has_children, data = samples)) # .
summary(lm(is_insured ~ maritalstatus_married, data = samples))
summary(lm(is_insured ~ maritalstatus_single, data = samples))

samples$gender <- as.factor(samples$gender)
summary(lm(is_insured ~ gender, data = samples)) # *

#Latinos or lower educational attainment?
samples$college_degree <- ifelse(samples$level_of_education %in% c("bach degree", "post graduate"), 1, 0)
samples$hs_degree <- ifelse(samples$level_of_education != "no hs degree", 1, 0)

summary(lm(college_degree ~ ethnicity_latino, data = samples)) # ***
summary(lm(hs_degree ~ ethnicity_latino, data = samples)) # ***

abline(lm(is_insured ~ ethnicity_latino, data = samples))
plot(is_insured ~ ethnicity_latino, data = samples)
