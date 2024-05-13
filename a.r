library(ggplot2)
data <- read.csv("~/Documents/Rproject-vote/hurricanes.csv")


data$wind <- as.numeric(data$wind)

data <- na.omit(data)

# Summary 
summary_stats <- summary(data)
print(summary_stats)

# Saffir simpson scale of wind intensity measurement
#wind_categories <- c("≥ 252", "209–251", "178–208", "154–177", "119–153", "63–118", "≤ 62")
intensity_levels <- c("5 - Danger", "4 - High", "3 - Moderate", "2 - Low", "1 - Very Low", "TS-Tropical Storm", "TD-Tropical Depression")

wind_speed <- cut(data$wind, 
              breaks = c(0, 62, 118, 153, 177, 208, 251, Inf), 
              labels = intensity_levels,right=FALSE)
barplot(table(wind_speed),
        main="Saffir siimpson scale of wind intensity measurement",
        xlab="Wind speed",
        ylab="Intensity level",
        col="skyblue",
        las = 0)
#axis(side = 2, at = seq(0, max(table(wind_speed)), by = 50), labels = seq(0, max(table(wind_speed)), by = 50))

# Time series plot of hurricane occurrences over the years
ggplot(data, aes(x = factor(year))) +
  geom_bar() +
  labs(title = "Hurricane Occurrences Over the Years",
       x = "Year",
       y = "Count")
  #scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 30))

# Boxplot of wind speeds by decade
data$decade <- as.factor(cut(data$year, breaks = seq(1970, 2030, by = 40), labels = seq(1970, 2020, by = 10)))
ggplot(data, aes(x = decade, y = wind)) +
  geom_boxplot() +
  labs(title = "Boxplot of Wind Speeds by Decade",
       x = "Decade",
       y = "Wind Speed")

# Histogram of wind speeds
ggplot(data, aes(x = wind)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Wind Speeds",
       x = "Wind Speed",
       y = "Frequency")

# Pie chart showing the proportion of hurricanes by category
category_counts <- table(cut(data$wind, breaks = c(0, 95, 110, 130, 155, Inf), labels = c("Category 1", "Category 2", "Category 3", "Category 4", "Category 5")))
pie(category_counts, main = "Proportion of Hurricanes by Category")
