library(ggplot2)
library(tidyverse)
library(scales)


#Importing the data
data <- read.csv("domain_data_after_data_cleaning.csv")


#Boxplot for Median total family income weekly and property price
fambin_size = 1000
data <- data %>% mutate(famincome_bins = paste0(fambin_size*floor(Median_tot_fam_inc_weekly/fambin_size),'-' ,fambin_size*floor(Median_tot_fam_inc_weekly/fambin_size) + fambin_size))

unique(data$famincome_bins)

data$famincome_bins <- factor(data$famincome_bins, levels = c("1000-2000", "2000-3000", "3000-4000", "4000-5000", "5000-6000"))

ggplot(data, aes(x= famincome_bins, y=price, fill = famincome_bins))+
  geom_boxplot()+
  scale_y_continuous(name="Property Price", labels = comma)+
  xlab('Median Total Family Income weekly')+ylab('Property Price')+
  theme_bw()+guides(fill=guide_legend(title="Median Total Family Income Weekly"))



#Boxplot for Median total personal income weekly and property price
pbin_size = 500
data <- data %>% mutate(pincome_bins = paste0(pbin_size*floor(Median_tot_prsnl_inc_weekly/pbin_size),'-' ,pbin_size*floor(Median_tot_prsnl_inc_weekly/pbin_size) + pbin_size))

unique(data$pincome_bins)

data$pincome_bins <- factor(data$pincome_bins, levels = c("0-500", "500-1000",  "1000-1500", "1500-2000", "2000-2500"))

ggplot(data, aes(x= pincome_bins, y=price, fill = pincome_bins))+
  geom_boxplot()+ scale_y_continuous(name="Property Price", labels = comma)+
  xlab('Median personal income weekly')+ylab('Property Price')+
  theme_bw()+guides(fill=guide_legend(title="Median Personal income weekly"))


#Boxplot for Median Mortgage Repayment Monthly and property price
repay_size = 1500
data <- data %>% mutate(repay_bins = paste0(repay_size*floor(Median_mortgage_repay_monthly/repay_size),'-' ,repay_size*floor(Median_mortgage_repay_monthly/repay_size) + repay_size))

unique(data$repay_bins)

data$repay_bins <- factor(data$repay_bins, levels = c("0-1500", "1500-3000", "3000-4500", "4500-6000","6000-7500", "7500-9000"))

ggplot(data, aes(x= repay_bins, y=price, fill = repay_bins))+
  geom_boxplot()+
  scale_y_continuous(name="Property Price", labels = comma)+
  xlab('Median Mortgage Repayment Monthly')+ylab('Property Price')+
  theme_bw()+guides(fill=guide_legend(title="Median Mortgage Repayment Monthly"))


# number of top school within 5km and mean property price
school_rank <- data %>% group_by(number_of_schools_within_5_km) %>% summarise(avg = mean(price))
school_rank


#line graph for number of top school within 5km and mean property price
ggplot(data = school_rank, aes(x = number_of_schools_within_5_km, y = avg, group=1)) +
  geom_line() + geom_point() + xlab('Number of Top Schools within 5km') + 
  ylab('Average Property Price') + theme_bw() 



#boxplot for distance from cbd and property price
bin_size = 5
data <- data %>% mutate(cbd_distance_bins = paste0(bin_size*floor(km_from_cbd/bin_size),'-' ,bin_size*floor(km_from_cbd/bin_size) + bin_size))

unique(data$cbd_distance_bins)

data$cbd_distance_bins <- factor(data$cbd_distance_bins, levels = c('0-5','5-10', '10-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50', '50-55', '55-60', '60-65', '65-70', '70-75', '75-80', '80-85'))

ggplot(data, aes(x= cbd_distance_bins, y=price, fill = cbd_distance_bins)) +
  geom_boxplot() + scale_y_continuous(name="Property Price", labels = comma) +
  xlab('Distance from CBD') + ylab('Property Price')+
  theme_bw() + guides(fill=guide_legend(title="Distance from CBD"))


#Boxplot for distance from Manly beach and property price
data <- data %>% mutate(manly_distance_bins = paste0(bin_size*floor(distance_from_Manly_Beach/bin_size),'-' ,bin_size*floor(distance_from_Manly_Beach/bin_size) + bin_size))

unique(data$manly_distance_bins)

data$manly_distance_bins <- factor(data$manly_distance_bins, levels = c('0-5','5-10', '10-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50', '50-55', '55-60', '60-65', '65-70', '70-75'))

ggplot(data, aes(x= manly_distance_bins, y=price, fill = manly_distance_bins)) +
  geom_boxplot() + scale_y_continuous(name="Property Price", labels = comma) +
  xlab('Distance from Manly Beach') + ylab('Property Price')+
  theme_bw() + guides(fill=guide_legend(title="Distance from Manly Beach"))



#boxplot for distance from coastline vs property price
data <- data %>% mutate(coastline_distance_bins = paste0(bin_size*floor(distance_from_coastline/bin_size),'-' ,bin_size*floor(distance_from_coastline/bin_size) + bin_size))

unique(data$coastline_distance_bins)

data$coastline_distance_bins <- factor(data$coastline_distance_bins, levels = c('0-5','5-10', '10-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45', '45-50', '50-55'))

ggplot(data, aes(x= coastline_distance_bins, y=price, fill = coastline_distance_bins)) +
  geom_boxplot() + scale_y_continuous(name="Property Price", labels = comma)+
  xlab('Distance from Coastline') + ylab('Property Price')+
  theme_bw() + guides(fill=guide_legend(title="Distance from Coastline"))


# correlation matrix for various distance attributes and price
nums <- unlist(lapply(data, is.numeric), use.names = FALSE)  
numeric_sales_listing_data <- data[, nums]
price_listing_data <- numeric_sales_listing_data[!is.na(numeric_sales_listing_data$price), ]
head(price_listing_data)
price_listing_data[c(2,10:24)]
demographics_data <- price_listing_data[c(2,10:24)]
melt(cor(demographics_data)) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_tile(color = 'black') + scale_fill_gradient2(low="red", high = "darkgreen", mid = "white",  name = "Pearson\nCorrelation")


