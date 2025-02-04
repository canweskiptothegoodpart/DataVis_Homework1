
#Part1.1
data <- read.csv("C:/Users/USER/OneDrive/Desktop/DataVis_Homework1/crime_data.csv")  
head(data, 5)




#Part1.2
missing_values <- colSums(is.na(data))
print(missing_values)
threshold <- nrow(data) * 0.5
data_clean <- data[, colSums(is.na(data)) <= threshold]
head(data_clean)
write.csv(data_clean, "crime_data_cleanR.csv", row.names = FALSE)





#Part1.3
data$DATE.OCC <- as.POSIXct(data$DATE.OCC, format="%m/%d/%Y %I:%M:%S %p")
data$year <- format(data$DATE.OCC, "%Y")
data$month <- format(data$DATE.OCC, "%m")
data$day <- format(data$DATE.OCC, "%d")
data$hour <- as.integer(substr(data$TIME.OCC, 1, 2))
data$minute <- as.integer(substr(data$TIME.OCC, 3, 4)) 
head(data)




#Part1.4
data_2023 <- subset(data, format(data$DATE.OCC, "%Y") == "2023")
data_burglary_2023 <- subset(data_2023, grepl("burglary", Crm.Cd.Desc, ignore.case = TRUE))
head(data_burglary_2023)




#Part1.5
library(dplyr)
crime_summary <- data %>%
  group_by(AREA.NAME) %>%
  summarise(
    total_crimes = n(),
    avg_victim_age = mean(Vict.Age, na.rm = TRUE)
  ) %>%
  arrange(desc(total_crimes))

print(crime_summary)




#Part3.1
library(dplyr)
crime_by_month <- data %>%
  group_by(month) %>%
  summarise(total_crimes = n()) %>%
  arrange(month)  
print(crime_by_month)



#Part3.2
library(dplyr)
weapon_crimes_count <- data %>%
  filter(!is.na(Weapon.Used.Cd)) %>%
  summarise(total_crimes_with_weapon = n())
print(weapon_crimes_count)



#Part3.3
library(dplyr)
crime_by_premises <- data %>%
  group_by(Premis.Desc) %>%
  summarise(total_crimes = n()) %>%
  arrange(desc(total_crimes)) 
print(crime_by_premises)





#Part4
library(dplyr)
data <- data %>%
  mutate(
    Severity_Score = case_when(
      !is.na(Weapon.Used.Cd) & grepl("burglary", Crm.Cd.Desc, ignore.case = TRUE) ~ 8, 
      !is.na(Weapon.Used.Cd) ~ 6,  
      grepl("burglary", Crm.Cd.Desc, ignore.case = TRUE) ~ 3, 
      TRUE ~ 1  
    )
  )
severity_by_area <- data %>%
  group_by(AREA.NAME) %>%
  summarise(total_severity_score = sum(Severity_Score, na.rm = TRUE)) %>%
  arrange(desc(total_severity_score))

print(severity_by_area)






#Bonus part
library(dplyr)

lat_min <- 34.0400  
lat_max <- 34.0600  
lon_min <- -118.2700
lon_max <- -118.2400 

downtown_crimes <- data %>%
  filter(LAT >= lat_min & LAT <= lat_max & LON >= lon_min & LON <= lon_max)
head(downtown_crimes)  









