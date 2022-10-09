library(tidyverse)

# Importing the data
sale_listing_data <- read.csv('sale_listing_data.csv')
station_data <- read.csv('StationEntrances2020_v4.csv')
school_location_data <- read.csv('school_location_dataset.csv')



# Function to find the geographic distance between two locations
find_distance <- function(lat1, lon1, lat2, lon2) {
  p <- 0.017453292519943295    # Math.PI / 180
  a <- 0.5 - cos((lat2 - lat1) * p)/2 + 
    cos(lat1 * p) * cos(lat2 * p) * 
    (1 - cos((lon2 - lon1) * p))/2
  
  return(12742 * asin(sqrt(a)))
} 


# Finding the distance from the listed property to the nearest train station

least_distance_to_train_stop <- c()
for (i in seq(1:nrow(sale_listing_data))){
  
  
  distance = find_distance(sale_listing_data$geoLocation.latitude[i],
                               sale_listing_data$geoLocation.longitude[i],
                               station_data$LAT,
                               station_data$LONG)
  
  least_distance_to_train_stop <- append(least_distance_to_train_stop, min(abs(distance)))
}

# adding a new column to store the distance from the nearest train station
sale_listing_data$distance_to_train <- least_distance_to_train_stop





# Segragateing the schools into primary school and secondary school
school_location_data %>% group_by(Level_of_schooling) %>% summarise(count=n())


# Finding the distance from the listed property to the nearest primary school. 
least_distance_to_primary_school <- c()
for (i in seq(1:nrow(sale_listing_data))){
  
  
  distance = find_distance(sale_listing_data$geoLocation.latitude[i],
                           sale_listing_data$geoLocation.longitude[i],
                           school_location_data[school_location_data$Level_of_schooling == 'Primary School', ]$Latitude,
                           school_location_data[school_location_data$Level_of_schooling == 'Primary School', ]$Longitude)
  
  least_distance_to_primary_school <- append(least_distance_to_primary_school, min(abs(distance)))
}


# adding a new column to store the distance from the nearest primary school
sale_listing_data$distance_to_primary_school <- least_distance_to_primary_school





# Finding the distance from the listed property to the nearest secondary school
least_distance_to_secondary_school <- c()
for (i in seq(1:nrow(sale_listing_data))){
  
  
  distance = find_distance(sale_listing_data$geoLocation.latitude[i],
                           sale_listing_data$geoLocation.longitude[i],
                           school_location_data[school_location_data$Level_of_schooling == 'Secondary School', ]$Latitude,
                           school_location_data[school_location_data$Level_of_schooling == 'Secondary School', ]$Longitude)
  
  least_distance_to_secondary_school <- append(least_distance_to_secondary_school, min(abs(distance)))
}

# adding a new column to store the distance from the nearest secondary school
sale_listing_data$distance_to_secondary_school <- least_distance_to_secondary_school


# Exporting the dataframe in CSV format.
write.csv(sale_listing_data,"sale_listing_data_with_distances.csv")
