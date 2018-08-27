require(dplyr)
require(ggplot2)
library(readr)
#color cheatsheet https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

################################### read in data ###########################################
setwd("~/county health rank 2018")
dat <- read_csv("chr_measures_CSV_2018.csv")
dat$state_county=paste(dat$county, dat$state, sep=" , ")
dat1=dat[which(!is.na(dat$county)),]

################################## functions ##############################################
#select related vars:
#function creating names from measure numbers: 
make_name=function(vars){
  out=rep(NA, length(vars))
  for ( i in 1:length(vars)){
    out[i]=paste0('measure_', vars[i], "_value") 
  }
  return(out)
}


############################ add names to original data and create set of vars ###############################################

###add names
#outcomes1: 
out_num1=c(127, 129, 128, 144, 145, 60, 61)
out_vars1=make_name(out_num1)
out_name1=c('premature_age_adjusted_mortality', 
            'infant_mortality',
            'child_mortality',
            'frequent_physical_distress',
            'frequent_mental_distress',
            'diabetes_prevalence',
            'HIV_prevalence')

#outcomes2: 
out_num2=c(1,2,36, 42, 37)
out_vars2=make_name(out_num2)
out_name2=c('premature_death',
            'poor_or_fair_health',
            'poor_physical_health_days',
            'poor_mental_health_days',
            'low_birthweight')

#measures:
#health behaviors: 
behavior_num1=c(139, 83, 39, 138, 146, 143)
behavior_vars1=make_name(behavior_num1)
behavior_name1=c('food_insecurity',
                 'limited_access_to_healthy_foods',
                 'motor_vehicle_crash_deaths',
                 'drug_overdose_deaths',
                 'drug_overdose_deaths_modeled',
                 'insufficient_sleep')

#clinical care: 
clinical_num1=c(3, 122, 86, 131)
clinical_vars1=make_name(clinical_num1)
clinical_name1=c('uninsured_adults',
                 'uninsured_children',
                 'health_care_costs',
                 "other_primary_care_providers")

#social and economic environment: 
social_num1=c(149, 63, 65, 15, 148, 141, 142)
social_vars1=make_name(social_num1)
social_name1=c('disconnected_youth',
               "median_household_income",
               'children_eligible_for_free_lunch',
               'homicides',
               'firearm_fatalities',
               'black_white_residential_segregation',
               'nonwhite_white_residential_segregation')

#demographics: 
demo_num=c(51, 52, 53, 54, 55, 81, 80, 56, 126, 59, 57, 58)
demo_vars=make_name(demo_num)
demo_names=c('population',
             'percent_age_below_18',
             'percent_65_and_older',
             'percent_non_hispanic_black',
             'percent_american_indian',
             'percent_asian',
             'percent_pacific_islander',
             'percent_hispanic',
             'percent_non_hispanic_white',
             'percent_english_nonproficent',
             'percent_female',
             'percent_rural')

#measures2:
#heath behavior 2
behavior_num2=c(9, 11,133,70,132,49,134,45,14)
behavior_vars2=make_name(behavior_num2)
behavior_name2=c('adult_smoking',
                 'adult_obesity',
                 'food_enviroment_index',
                 'physical_inactivity',
                 'access_to_exercise_opportunities',
                 'excessive_drinking',
                 'alcohol_impaired_driving_deaths',
                 'STD',
                 'teen_births')

clinical_num2=c(85,4,88,62,5,7,50)
clinical_vars2=make_name(clinical_num2)
clinical_name2=c('uninsured',
                 'Primary_Care_physicians',
                 'dentist',
                 'mental_health_providers',
                 'preventable_hospital_stays',
                 'diabetes_monitoring',
                 'mammography_screening')

social_num2=c(21,69,23,24,44,82,140,43,135)
social_vars2=make_name(social_num2)
social_name2=c('high_school_graduation',
               'some_college',
               'unemployment',
               'children_in_poverty',
               'income_inequality',
               'children_in_single_parent_households',
               'social_associations',
               'violent_crime',
               'injury_deaths')

physical_num=c(125,124,136,67,137)
physical_vars=make_name(physical_num)
physical_name=c('air_pollution',
                'drinking_water_violations',
                'severe_housing_problems',
                'driving_alone_to_work',
                'long_commute_driving_alone')

out=c(out_vars1, out_vars2)
behaviors=c(behavior_vars1, behavior_vars2)
clinicals=c(clinical_vars1,clinical_vars2)
socials=c(social_vars1, social_vars2)
features=c(behaviors, clinicals, socials,physical_vars, demo_vars)#add all features together

vars=c(out,features,"5-Digit FIPS Code",'state_county') #add all vars needed together


#dat2 has all vars needed. it disregarded all vars not needed
dat2=dat1[,vars]
name=c(out_name1, out_name2, behavior_name1, behavior_name2, clinical_name1, clinical_name2, 
       social_name1, social_name2, physical_name, demo_names) #put all names together
colnames(dat2)[1:71]=name #change all names except geo vars

write.csv(dat2, 'cleaned.csv')
#put feature names, and their categories together 
#feature_name=name[13:71] #extract feature names so feature_name is same as features except the names are different 
#behavior_name=c(behavior_name1, behavior_name2)
#clinical_name=c(clinical_name1, clinical_name2)
#social_name=c(social_name1, social_name2)
#physical_name
#demo_name


