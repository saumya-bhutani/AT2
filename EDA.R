library(DAAG)
library(MASS)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(psych)
library(RColorBrewer)
library(reshape2)

#Importing the data
domain_data <- read.csv('domain_properties_merged.csv')

domain_data$cash_rate <- NULL
domain_data$property_inflation_index <- NULL
domain_data$X <- NULL

#creating correlation matrix
nums <- unlist(lapply(domain_data, is.numeric), use.names = FALSE)  
numeric_data <- domain_data[, nums]
price_listing_data <- numeric_data[!is.na(numeric_data$price), ]
melt(cor(price_listing_data)) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_tile()

#Suburb Median Income vs Price
domain_data %>% ggplot(aes(x = suburb_median_income , y = price)) +
  geom_point(color = 'dark green') + geom_smooth(se = 0, method = 'lm') +
  scale_y_continuous(labels = scales::comma) +  theme_bw() +
  ggtitle('Suburb median income vs Price') + xlab("Suburb median income") +
  ylab("Price")

#Distance between train station vs price
domain_data %>% ggplot(aes(x = distance_to_train , y = price)) +
  geom_point(color = 'red') +
  scale_y_continuous(labels = scales::comma) + theme_bw() + 
  ggtitle('Distance from Train Station vs Price') + 
  xlab("Distance from Train Station") + ylab("Price")

#Primary school distance vs price
domain_data %>% ggplot(aes(x = distance_to_primary_school , y = price)) +
  geom_point(color = 'red') + scale_y_continuous(labels = scales::comma) +
  ggtitle('Distance from Primary School vs Price') +  theme_bw() +
  xlab("Distance from Primary School") + ylab("Price")

#Secondary school distance vs price
domain_data %>% ggplot(aes(x = distance_to_secondary_school , y = price)) +
  geom_point(color = 'red') + scale_y_continuous(labels = scales::comma) + theme_bw() + 
  ggtitle('Distance from Secondary School vs Price') + xlab("Distance from Secondary School") + ylab("Price")

#Distance from cbd vs price
domain_data %>% ggplot(aes(x = km_from_cbd , y = price)) +
  geom_point(color = 'blue') + scale_y_continuous(labels = scales::comma) +
  ggtitle('Distance from CBD vs Price') +  theme_bw() + 
  xlab("Distance from CBD") + ylab("Price")






