# == LOAD LIBRARIES == #
library(ggplot2)
library(ggmap)
library(dplyr)
library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(ggpubr)
library(grid)
library(leaflet)
library(tmap)
library(RColorBrewer)
library(lctools)
library(stringi)
library(dplyr)


# == EDA of Airbnb == #

# Read the data table
df = read.csv('listings.csv')

df %>% 
  mutate(popup = stri_c('Neighbourhood: ',  neighbourhood_group, '<br>',
                  'Price:', price, '<br>',
                  'Room Type: ', room_type, '<br>', 
                  'Minimum Stay:', minimum_nights, '<br>',
                  'Availability:', availability_365),
                  'Latitude:', latitude,
                  'Longitude:', longitude) %>% 
  leaflet() %>%   addTiles() %>%
  addMarkers(popup=~popup, clusterOptions = markerClusterOptions())

# Column names
names(df)

# Length of dataset
length(unique(df$id))

# Display the structure of the df
str(df)


# == 1. No. of listings by neighborhood == #
ggplot(df) + 
  geom_histogram(aes(neighbourhood_group,fill=neighbourhood_group),
stat = "count",alpha = 0.85)+
  theme_minimal(base_size=13) + xlab("") + ylab("") +theme(legend.position="none") + 
  ggtitle("The Number of Property in Each Area")

df %>% ggplot(aes(x = longitude, y = latitude, 
                  color= neighbourhood_group)) + 
  geom_point(alpha = 0.4) 

# == 2. Room Type in Each Area == #
ggplot(df) + geom_histogram(aes(room_type, fill = room_type), stat = "count",alpha = 0.85) + 
  theme_minimal(base_size=13) + 
  xlab("Room type") + ylab("No. of Listings")  + 
  ggtitle("The Proportion of Room Type in Each Area")

df %>% ggplot(aes(x = longitude, y = latitude, 
                  color= room_type)) + 
  geom_point(alpha = 0.4)

# == 3. Combine the above graphs == #
ggplot(df) + geom_histogram(aes(neighbourhood_group, fill = room_type), stat = "count",alpha = 0.85, position = 'fill') + 
  theme_minimal(base_size=13) +
  xlab("Neighbourhoods") + ylab("Proposion")  + 
  ggtitle("The Proportion of Room Type in Each Area")

# == 4. Top 10 neighborhoods by no. of listings == #
n = table(df$neighbourhood)
n = tail(sort(n, decreasing = FALSE), 10)
barplot(n, main = 'Top 10 Neighbothoods',
        horiz = TRUE,
        xlab = 'No. of listings',
        col = '#69b3a2', las = 2)


# == 5. Price by room type == #
ggplot(df, aes(x = room_type, y = price, 
               col = room_type))+
  geom_boxplot()+
  scale_y_log10()

# == 6. Distribution of Listing Price  == #
ggplot(df)+ 
  geom_histogram(aes(price),fill = '#fd5c63',alpha = 0.85, binwidth = 10) +
  theme_minimal(base_size = 15)+
  xlab('Price')+
  ylab('Number')+
  xlim(0,500)+
  ggtitle('The distribution of Price')

# == 7. Mean Price in each Area  == #
meanp = df%>%
  group_by(neighbourhood_group)%>%
  summarise(price = round(mean(price),2))
meanp           

# == 8. Relationship Number of Reviews - Price  == #
ggplot(df, aes(number_of_reviews, price))+
  geom_point(aes(size = price),alpha = .05, 
             color = 'slateblue')+
  xlab('Number of reviews')+
  ylab('Price')+
  ggtitle('Relationship between number of reviews')

# == 9. 150 most expensive listings  == #
top_df = df %>% top_n(n = 150, wt = price)

# == 10. 150 most cheap listings  == #
least_df = df %>% top_n(n = -150, wt = price)

# == 11. Median Price by Neighborhood  == #
med = df %>%
  group_by(neighbourhood) %>%
  summarise(num_listings = n(),
            median_price = median(price),
            long = median(longitude),
            lat = median(latitude),
            borough = unique(neighbourhood_group))




# == MAPS == #

# SETTING A COORDINATE SYSTEM

# Coordinate system for top listings / plot
coordinates(top_df) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(top_df) = crs.geo1 

plot(top_df,pch = 20, col = 'red')

# Coordinate system for least listings / plot
coordinates(least_df) = c("longitude","latitude")
crs.geo1 = CRS("+proj=longlat")  
proj4string(least_df) = crs.geo1 

plot(least_df,pch = 20, col = 'purple')

# Coordinate system for mean value / plot
coordinates(med) = c("long","lat")
crs.geo1 = CRS("+proj=longlat")  
proj4string(med) = crs.geo1 

plot(med,pch = 20, col = 'blue')


# READ THE SHAPEFILE OF NEW YORK
p = readOGR('Zoning GIS Data_ Shapefile')
plot(p)

# CREATE COLOURED MAPS
# Creates a coloured dot map of top 150 listings
tm_shape(p) + tm_borders(alpha=.4)+
  tm_shape(top_df)+
  tm_dots(col = "price",
          scale = 5,
          palette = "Reds", 
          style = "quantile",
          title = ' Top Prices per Night ($)')+
  tm_shape(least_df)+
  tm_dots(col = "price",
          scale = 5,
          palette = "Greens", 
          style = "quantile",
          title = 'Least Prices per Night ($)')+
  tm_compass()


# Creates a coloured dot map of median price
tm_shape(p) + 
  tm_borders(alpha=.4)+
  tm_shape(med) + 
  tm_dots(col = "median_price",
          scale = 5,
          palette = "Purples", 
          style = "quantile",
          title = 'Median Price ($)')+
  tm_compass()



zipReviews <- df %>% group_by(zipcode = neighbourhood_group) %>% summarise(avg_loc_review = mean(review_scores_location, na.rm = TRUE))
colnames(zipReviews) <- c("region","value")
zipReviews$region <- as.character(zipReviews$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
g_locations <- zip_choropleth(zipReviews,
                              county_zoom = nyc_fips,
                              title = "Location Review Scores by Region",
                              legend = "Average Score") + ggtitle("Which area is the best?",
                                                                  subtitle = "Map showing Average Location Score by Area") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+ scale_fill_brewer("Location Review Score",palette=3)
g_locations



