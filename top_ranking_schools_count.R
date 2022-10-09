library(tidyverse)
library(readxl)


school_data <- read_excel('school_ranking.xlsx')
listing_data <- read.csv('domain_properties_merged_1.csv')

# Function to find the geographic distance between two locations
find_distance <- function(lat1, lon1, lat2, lon2) {
  p <- 0.017453292519943295    # Math.PI / 180
  a <- 0.5 - cos((lat2 - lat1) * p)/2 + 
    cos(lat1 * p) * cos(lat2 * p) * 
    (1 - cos((lon2 - lon1) * p))/2
  
  return(12742 * asin(sqrt(a)))
} 


number_of_schools_within_5_km <- data.frame(matrix(nrow = nrow(school_data), ncol = 1))

for(i in 1:nrow(listing_data)){
  
  
  
  distances_from_school <- find_distance(listing_data$suburb_lat[i],
                listing_data$suburb_lng[i],
                school_data$lat,
                school_data$long)
  number_of_schools_within_5_km[i,1] = sum(distances_from_school < 5)
}


number_of_schools_within_5_km


listing_data <- cbind(listing_data, number_of_schools_within_5_km)
listing_data$X.1 <- NULL
listing_data$X <- NULL

colnames(listing_data)[ncol(listing_data)] <- 'number_of_schools_within_5_km'

write.csv(listing_data, 'domain_properties_merged_2.csv')
