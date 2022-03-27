# Clear the enviroment
rm(list = ls())

# Load or install libraries
libraries_list = c("tidyverse", "leaflet", "dygraphs", "xts", "sp", "sf", "plotly", "rgdal", "geojsonio", "viridis", "")
new_librariss <- libraries_list[!(libraries_list %in% installed.packages()[,"Package"])]
if(length(new_librariss)) install.packages(new_librariss) # Aqui instalo las librerias que no estan
lapply(libraries_list, library, character.only = TRUE) # Aqui cargo los paquetes


# Set the current directory as the working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the lite file
df = read_csv("data/ppdata_lite.csv")
str(df)

# Making sense of the data ------------------------------------------------

# Descriptive statistics of the columms
summary(df)

# Geographical data
print(length(unique(df$county)))
print(length(unique(df$district)))
print(length(unique(df$town_city)))
print(length(unique(df$locality)))
print(length(unique(df$postcode)))

# London -------------------------------------------------------------------

london_df = df %>% filter(county == 'GREATER LONDON') # Data set for only London
london_df_2015 = london_df %>% filter(format(date_of_transfer, format = "%Y") == 2015) # Data set for only London
print(length(unique(london_df$district))) # Print the number of districts also called Boroughs

# It's important to plot the average value for district so I create a new table with that information 
london_avg_p = london_df_2015 %>% group_by(district) %>% summarise(price = mean(price)) %>% arrange(price)

## House prices between Boroughs?
ggplot(data = london_df_2015, 
       aes(x = fct_relevel(district, rev(london_avg_p$district)), # In this line I indicate the order which is the reverse of the values in london_avg_p
           y = price)) + 
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) + 
  geom_point(data = london_avg_p, aes(x = district, y = price, color = 'red'), size = 1) + 
  coord_flip() +
  scale_y_log10() + 
  labs(x = "London Boroughs", 
       y = "Log of Price", 
       color = "Mean Price") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))

# Box plots are good but violin plots can be better
ggplot(data = london_df_2015, aes(x = district, y = price)) + 
  geom_violin(adata = london_avg_p, aes(fill = price)) + 
  geom_point(data = london_avg_p, aes(x = district, y = price), size = 1, color = "red") + 
  scale_y_log10() + 
  coord_flip() +
  labs(x = "London Boroughs", 
       y = "Log of Price") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))

## Relationship between price and floor levels
### Create the floor level (only for flats (property_type == "F"))) by extracting the information from SAON

#### The next pipeline extracts as well as possible the information of the floor by the following proccess
  #### 1 Using the strings to denote floors from basement to 9th floor
  #### Taking the first digit in the case of floor numbers of two digits
  #### Taking the first two digits in the case of floor numbers of three digits
  #### Select up to 19 floors levels

floor_price_df = df %>% 
  filter(property_type == "F" & format(date_of_transfer, format = "%Y") == 2015) %>%
  mutate(Floor_x = str_replace_all(SAON, c(".*BASEMENT.*" = "-1",
                                         ".*GROUND.*" = "0",
                                         ".*FIRST.*" = "1",
                                         ".*SECOND*" = "2",
                                         ".*THIRD*" = "3",
                                         ".*FOURTH*" = "4",
                                         ".*FIFTH*" = "5",
                                         ".*SIXTH*" = "6",
                                         ".*SEVENTH*" = "7",
                                         ".*EIGHTH*" = "8",
                                         ".*NINETH*" = "9",
                                         "FLAT " = "",
                                         "APARTMENT " = ""))) %>%
  mutate(Floor_x = as.character(as.numeric(Floor_x))) %>% 
  mutate(Floor_x = case_when(nchar(Floor_x) == 1 ~ Floor_x,
                           nchar(Floor_x) == 2 & Floor_x != "-1" ~ str_sub(Floor_x, 1, 1),
                           nchar(Floor_x) == 3 ~ str_sub(Floor_x, 1, 2)
                           )) %>% 
  mutate(Floor = as.numeric(Floor_x)) %>%
  select(property_type, SAON, Floor, price) %>%
  filter(Floor < 20) %>%
  drop_na(Floor)

# Run a regression between floor level a price
lm1 = lm(log(price) ~ Floor, floor_price_df)
reg_text = paste("log(price) =", round(lm1$coefficients[1],2), "+", round(lm1$coefficients[2],3), "floor_level")

# Create a p graph with an annotation of the regression results
## In this kind of situations is better to work with jitter isntead of point (geom_point)

ggplot(data = floor_price_df, aes(x = Floor, y = price)) +
  scale_y_log10() +
  geom_jitter(alpha = 0.2, size = 0.5) +
  geom_smooth(method='lm', formula = y ~ x) + 
  annotate("text", x = 13, y = 1e+07, label = reg_text) +
  labs(x = "Floor level", 
       y = "Log of Price") +
  theme_minimal()



# Geo plots ---------------------------------------------------------------

## Load the uk geo zip codes with longitude and latitude to create maps
uk_postcodes_list <- read.csv("data/ukpostcodes.csv", header = TRUE, sep = ',')
print(length(unique(uk_postcodes_list$postcode)))

## Load UK shape files
uk_shapefiles2 <- st_read("data/shapefiles/postcode_shapefile.shp")
uk_shapefiles = readOGR(dsn = file.path("data/shapefiles", "postcode_shapefile.shp"), stringsAsFactors = F)

shp_trans <- st_transform(uk_shapefiles2, 2163)

# Create a dataframe
uk_shapefiles = broom::tidy(uk_shapefiles, region = "pc_distric")

# Create a dataframe with the information of prices and the shape data of 2015 by district
res_price_df = df %>%
        mutate(year_transfer = format(date_of_transfer, format = "%Y"),
               pc_distric = str_sub(postcode,1,3)) %>%
        filter(year_transfer == 2015) %>%
        group_by(pc_distric) %>%
        summarise(min_price = min(price),
                  max_price = max(price),
                  med_price = median(price),
                  avg_price = mean(price)) %>%
        left_join(uk_shapefiles %>% 
                    rename(pc_distric = id) , by = "pc_distric")
        
summary(res_price_df)

# Map

map1 = ggplot(gadm36_DEU_2_shp_df, 
              aes(long, lat, group = group)) +
  geom_polygon(aes(fill = `Number of power stations`)) + 
  coord_quickmap() + 
  theme_void()  


ggplotly(ggplot(res_price_df
                aes()) +
           geom_sf(aes(geometry = geometry, fill=log(med_price), 
                       text=pc_distric))+
           coord_sf() +
           #scale_fill_viridis(direction = -1) + 
           #scale_color_viridis(direction = -1) +
           ggtitle(paste("Median price by Postcode District in the UK"))+
           labs(fill=paste("Log. Median Price"))+
           theme_bw()+
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(), 
                 axis.text = element_blank(), 
                 axis.ticks = element_blank(), 
                 plot.title = element_text(hjust = 0.5, face = "bold")),
         height = 800, width=800)

  

## Add to the main data frame the information of long and lat from ukpostcodes
### at the same time group the information by zip zone (code), summarize prices,
### and getting the center values of lat and long for each zip code

res_price_df = df %>%
            mutate(year_transfer = format(date_of_transfer, format = "%Y")) %>%
            filter(year_transfer == 2015) %>%
            left_join(uk_postcodes_list %>% select(-id), by = "postcode") %>%
            group_by(postcode) %>%
            summarise(min_price = min(price),
                      max_price = max(price),
                      med_price = median(price),
                      avg_price = mean(price),
                      latitude = min(latitude),
                      longitude = min(longitude))
            
      
m <- leaflet(london_df_s %>% filter(year_transfer == 2015) %>% head(20)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=0.1276, lat=51.5073, zoom = 8) %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = ~paste("Median house price", "<br>",
                            as.character(med_price), "<br><br>",
                            "Min house price", "<br>",
                            as.character(min_price), "<br><br>",
                            "Max house price", "<br>",
                            as.character(max_price), "<br><br>",
                            "Average house price", "<br>",
                            as.character(avg_price), "<br><br>"))
              




#############
# House prices throughout 2015

london_df = london_df %>% 
              mutate(year_transfer = format(date_of_transfer, format = "%Y"),
                     month_transfer = format(date_of_transfer, format = "%m")) %>% 
              mutate(season = case_when(month_transfer %in% c("01", "02", "03") ~ "winter",
                                        month_transfer %in% c("04", "05", "06") ~ "spring",
                                        month_transfer %in% c("07", "08", "09") ~ "summer",
                                        month_transfer %in% c("10", "11", "12") ~ "fall"))

london_df_day_15 = london_df %>%
                filter(year_transfer == 2015) %>%
                filter(!(abs(price - median(price)) > 2 * sd(price))) %>%
                group_by(year_transfer, season, month_transfer, date_of_transfer) %>%
                summarise(price = median(price), .groups = "keep") 
    

london_df_month_15 = london_df %>%
  filter(year_transfer == 2015) %>%
  mutate(month_transfer = as.Date(date_of_transfer, format = "%Y-%b-01")) %>%
  group_by(year_transfer, season, month_transfer) %>%
  summarise(price = median(price), .groups = "keep") %>%
  mutate(month_transfer = as.Date(month_transfer, format = ""))

# Relation between month of the year and price
ggplot(data = london_df %>% filter(year_transfer == 2015), 
       aes(x = month_transfer, y = price)) +
  scale_y_log10() +
  geom_boxplot()

# Season and price
ggplot(data = london_df %>% filter(year_transfer == 2015), 
       aes(x = season, y = price)) +
  scale_y_log10() +
  geom_boxplot()

# Trend for the year
ggplot(data = london_df_day_15, aes(x = as.Date(date_of_transfer), y = price)) + 
  geom_line() + 
  scale_y_log10() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") 

# https://stackoverflow.com/questions/59119580/in-ggplot-how-can-i-plot-data-as-a-line-graph


# Plot with dygraph

ts_london = xts(london_df %>% select(date_of_transfer, price), order.by = london_df$date_of_transfer)

dygraph(ts_london)





london_df_day %>% select(date_of_transfer, price)
