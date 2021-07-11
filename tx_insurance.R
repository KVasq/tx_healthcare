library(dplyr)

#load csvs

insurance_rates <- read.csv("tx_county_insurance_rates.csv")
demographics <- read.csv("tx_county_summary.csv")
samples <- read.csv("data_day_sample_20180410_export.csv")
county_region <- read.csv("county_to_media_region.csv")

county_age <- read.csv("alldata.csv")

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

# Option 1 - Outreach Strategy to reach as many uninsured consumers as possible
# Step 1. Identify which mode will reach the most amount of uninsured people with the least money
# - Identify which county has the most uninsured people: 
# Step 2. Plan to add a new mode and decide amount of allocation to both
# Step 3. Decide test to inform allocation plan 
# Buy a TV ad block and mail for 5000 people and see which one has a higher turnover rate.

county <- merge(insurance_rates, county_key, by.x = "county", by.y = "censuskey")

county_region$demo_county_name <- toupper(county_region$demo_county_name) 
county <- merge(county, county_region, by.x = "county_name", by.y = "demo_county_name")

#Identify region with most uninsured people

by_region <- group_by(county, tx_media_region)
by_region %>% summarize(pct.insurance = mean(as.numeric(pct.insurance)))

#TV - We can afford 20 ad blocks in the cheapest region, reaching 100,000 people

#Mail - Mailing only 1 county let's us afford to mail 96,000 of which we can assume 72,000 will receive

#FB - The cheapest bucket lets us reach 200,000 people, the older people are the more likely they are to be insured
# those above 65 are around 10-12% likely to be uninsured



# Option 2 - Design data pipeline to track people's registration over time
# Step 1. Decide type of database and data loading method
# Step 2. Decide how to automate data analysis process so key metrics can be reviewed any time
