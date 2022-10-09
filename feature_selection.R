# Libraries
install.packages("Boruta")
install.packages("mlbench")
install.packages("caret")
library(Boruta)
library(mlbench)
library(caret)

# Data
domaindata <- read.csv("C:/Users/saumy/OneDrive/Desktop/STDS/Final Dataset/domain_properties_merged_4.csv")
str(domaindata)
mean(domaindata$price)

domaindata %>% ggplot(aes(x = suburb , y = price)) +
  geom_point(color = 'dark green') + 
  scale_y_continuous(labels = scales::comma) + theme_bw() + ggtitle('Distance from CBD vs Price') + 
  xlab("Distance from CBD") + ylab("Price") +   facet_wrap(~ suburb)


domian_data %＞% ggplot(aes(x = distance_from_coastline, y = price)) +
  geom_histogram(bins=10, color = "black", fill="gray90")


# Feature Selection
set.seed(111)
boruta <- Boruta(price ~ ., data = domain_data, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

