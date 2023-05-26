#### QuantUX Conference 2023 ####
# Session: QuantUXR to QuantCritUXR: Strategies for Centering Underrepresented Individuals in Research
# Presenters: Kyle Fassett & Tyler Steelman
# Contact: kfassett@unc.edu & tsteelman@unc.edu
# Date: June 14, 2023

#### Begin Script ####
# Packages needed
install.packages("tidyverse")
library(tidyverse) 

install.packages('wakefield')
library(wakefield)  # for data creation

install.packages("lme4")
library(lme4) # for regression

# Set a seed to make the dataframe creation consistent
set.seed(1111)

# Create fake data
# note: the categories are pre-established thus may not be the most current vernacular
df <- r_data_frame(n = 500, 
               id, 
               race, 
               age(x = 18:30), 
               gender_inclusive,
               income) %>% 
  mutate(Satisfaction = sample(1:10, 500, replace= T))


#### Effect Coding ####
# Effect Code (aka contrast coding/grand mean centering) Race variable to avoid leaving one group out as dummy code reference category
# take a peek at the categories to know what we have to recode
table(df$Race)

# recode to create the 1st set of effect codes (e1); we do this twice and leave a different group out each time to get all the coefficients in our model in the end
# we arbitrarily leave out 'Hawaiian' as -1 for this round to help create the averages; chose this group because it was the last group listed
# we'll abitrarily leave 'White' out for our 2nd set of effect codes since it is listed first 
# notice the -1, 0, 1 pattern. 

df$white_e1<-recode(df$Race, 
                        'White'=1, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=-1)
df$hispanic_e1<-recode(df$Race, 
                        'White'=0, 'Hispanic'=1, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=-1)
df$black_e1<-recode(df$Race, 
                        'White'=0, 'Hispanic'=0, 'Black'=1, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=-1)
df$asian_e1<-recode(df$Race, 
                        'White'=0, 'Hispanic'=0, 'Black'=0, 'Asian' = 1, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=-1)
df$biracial_e1<-recode(df$Race, 
                        'White'=0, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=1, 'Native'=0, 'Other'=0, 'Hawaiian'=-1)
df$native_e1<-recode(df$Race, 
                        'White'=0, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=1, 'Other'=0, 'Hawaiian'=-1)
df$other_e1<-recode(df$Race, 
                        'White'=0, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=1, 'Hawaiian'=-1)

# list of variables we created for our model: white_e1 + hispanic_e1 + black_e1 + asian_e1 + biracial_e1 + native_e1 + other_e1


# recode to create the 2nd set of effect codes (e2)
# we arbitrarily leave out 'White' (-1) for this round to help create the averages
df$hispanic_e2<-recode(df$Race, 
                          'White'=-1, 'Hispanic'=1, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=0)
df$black_e2<-recode(df$Race, 
                           'White'=-1, 'Hispanic'=0, 'Black'=1, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=0)
df$asian_e2<-recode(df$Race, 
                           'White'=-1, 'Hispanic'=0, 'Black'=0, 'Asian' = 1, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=0)
df$biracial_e2<-recode(df$Race, 
                           'White'=-1, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=1, 'Native'=0, 'Other'=0, 'Hawaiian'=0)
df$native_e2<-recode(df$Race, 
                           'White'=-1, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=1, 'Other'=0, 'Hawaiian'=0)
df$other_e2<-recode(df$Race, 
                           'White'=-1, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=1, 'Hawaiian'=0)
df$hawaiian_e2<-recode(df$Race, 
                           'White'=-1, 'Hispanic'=0, 'Black'=0, 'Asian' = 0, 'Bi-Racial'=0, 'Native'=0, 'Other'=0, 'Hawaiian'=1)

# list of variables we created for our model: hispanic_e2 + black_e2 + asian_e2 + biracial_e2 + native_e2 + other_e2 + hawaiian_e2


#### Regression Modeling #### 

### Dummy coded models

# Model 1 (m1) is a regression model with dummy codes; we'll run this to see how the results compare to our effect coded models
m1<-lm(Satisfaction ~ factor(Race), 
       data = df)

summary(m1)

# Result: we see Asian (B=1.21, p<.05) reported more satisfaction while Hispanic (B=-0.86, p<.05) reported less in comparison to their White peers.
# The narrative focuses on juxtaposition to White individuals. 

### Effect coded models 
# we have to run the model twice, with both sets of effect codes in order to get all of the Race coefficients 
# if we do it correctly, you'll notice the coefficients for the overlapping categories are the same (e.g., Hispanic, Black, Asian, Native, Other)
# Then, we pull 'White' from model 1, and 'Hawaiian' from model 2 to have a complete set of coefficients for all Race categories

# Effect Code Model 1 
m_e1<-lm(Satisfaction ~ white_e1 + hispanic_e1 + black_e1 + asian_e1 + biracial_e1 + native_e1 + other_e1 ,
          data= df)

summary(m_e1)


# Effect Code Model 2 
m_e2<-lm(Satisfaction ~ hispanic_e2 + black_e2 + asian_e2 + biracial_e2 + native_e2 + other_e2 + hawaiian_e2 ,
          data= df)

summary(m_e2)

# Result: the results for the overlapping race categories should be the same for both regressions then we add in the 'left out' categories to get 
# a complete set of results for all race categories
# we see there are no practical statistical differences when we compare race categories to the average user experience
# depending the research question this could be good; sometimes no differences are good findings (e.g., everyone has the same satisfaction levels with the product)

# disclaimer: this is a perfect solution because if the population is majority White then the average will be comprised of majority White responses


#### End of Script ####

