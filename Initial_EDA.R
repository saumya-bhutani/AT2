library(DAAG)
library(MASS)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(psych)


#dataset

domaindata <- read.csv("C:/Users/saumy/OneDrive/Desktop/STDS/sale_listing_data_with_distances.csv")

str(domaindata)
summary(domaindata)

#density plot
domain_data %>% ggplot(aes(x = bedrooms, fill = propertyType )) +
  geom_density(alpha = 0.5) +
  ggtitle('Distribution of no of bedrooms', 'Source: DAAG')

#scatter plot
# Age vs household size
domaindata %>% ggplot(aes(x = Median_age_persons, y = Average_household_size)) +
  geom_point(color = 'dark green') +
  geom_smooth() + ggtitle('Median age vs Average household size')
# final 1
# Personal weekly income vs price
domaindata %>% ggplot(aes(x = Median_tot_prsnl_inc_weekly , y = price)) +
  geom_point(color = 'red') + geom_smooth(se = 0, method = 'lm') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Personal Weekly Income vs Price') + xlab("Personal Weekly Income") +
  ylab("Price")

#  house type vs price 
domaindata %>% ggplot(aes(x = propertyType, y = price, cols = propertyType)) +
  geom_boxplot(color = 'purple') +
  geom_smooth(se = 0, method = 'lm') + scale_y_continuous(labels = scales::comma) +
  ggtitle('Property type vs price')

# distance between train station vs price
domaindata %>% ggplot(aes(x = distance_to_train , y = price)) +
geom_point(color = 'red') +
scale_y_continuous(labels = scales::comma) + theme_bw() + ggtitle('Distance from Train Station vs Price') + 
 xlab("Distance from Train Station") + ylab("Price")


#primary school distance vs price
domaindata %>% ggplot(aes(x = distance_to_primary_school , y = price)) +
  geom_point(color = 'red') + scale_y_continuous(labels = scales::comma) +
  ggtitle('Distance from Primary School vs Price') +  theme_bw() + 
  xlab("Distance from Primary School") + ylab("Price")

#secondary school distance vs price
domaindata %>% ggplot(aes(x = distance_to_secondary_school , y = price)) +
  geom_point(color = 'red') + scale_y_continuous(labels = scales::comma) + theme_bw() + 
  ggtitle('Distance from Secondary School vs Price') + xlab("Distance from Secondary School") + ylab("Price")

#carspaces vs price
domaindata %>% ggplot(aes(x = carspaces , y = price)) +
  geom_point(color = 'blue') +
  geom_smooth(se = 0)  + scale_y_continuous(labels = scales::comma) + ggtitle('Number of carspaces vs price')


#Median age vs Median personal income weekly
p <- domaindata %>%
  ggplot(aes(x = Median_age_persons , y = Median_tot_prsnl_inc_weekly)) +
  geom_point(color = 'blue') +
  geom_smooth(se = 0, method = 'lm')  + scale_y_continuous(labels = scales::comma) +
  ggtitle('Median age vs Median personal income weekly', 'Source: DAAG')
  ggMarginal(p, fill = 'pink', type = 'histogram', alpha = 0.5)

  #bedroom vs price
domaindata %>% ggplot(aes(x = bedrooms , y = price)) +
    geom_point(color = 'blue') +
    geom_smooth(se = 0)  + scale_y_continuous(labels = scales::comma) + ggtitle('Number of carspaces vs price')
  

                                                          