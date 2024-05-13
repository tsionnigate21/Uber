# Uber Trips Analysis

This project analyzes Uber trips data to uncover insights and patterns in the data. It includes visualizations and a Shiny app for interactive exploration of the data.

## Introduction
This project uses R programming language along with various libraries such as `dplyr`, `ggplot2`, `leaflet`, and `shiny` for data processing, visualization, and interactive analysis.

### Cleaning the Data
  Step 1:
  ```
  data04 <- read.csv("~/Documents/DATA/Data 332/files given/Uber data/uber-raw-data-apr14.csv")
  data05 <- read.csv("~/Documents/DATA/Data 332/files given/Uber data/uber-raw-data-may14.csv")
  data06 <- read.csv("~/Documents/DATA/Data 332/files given/Uber data/uber-raw-data-jun14.csv")
  data07 <- read.csv("~/Documents/DATA/Data 332/files given/Uber data/uber-raw-data-jul14.csv")
  data08 <- read.csv("~/Documents/DATA/Data 332/files given/Uber data/uber-raw-data-aug14.csv")
  data09 <- read.csv("~/Documents/DATA/Data 332/files given/Uber data/uber-raw-data-sep14.csv")
 Bind all the data together
  combined_data <- bind_rows(data04, data05, data05, data07, data08, data09)
  ```  
  2: Changing the date column to a date schema
```
  combined_data$Date.Time <- as.POSIXct(combined_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
```
# Data Analysis 
- Interactive controls to filter data by day of the week and month.
```
 h3("Controls"),
      checkboxGroupInput("dayOfWeekSelector", "Select Days of the Week:",
```
- Visualizations including line graphs, bar charts, and heatmaps.
  
- Geospatial mapping of trip densities using Leaflet.
  
- Tables displaying trip data per day.
  
- The analysis explores Uber trip data to gain insights into patterns of usage over time. The transformed data is then used to create

- several visualizations including a bar graph of trips by hour, a bar graph of trips by hour and month, a bar graph of trips by day, and a bar graph of trips by day and month. These visualizations provide an understanding of when Uber is most frequently used and how usage patterns vary by day, hour, and month.

## Usage
The Shiny app provides various tabs for different visualizations and analyses of Uber trips data:
Define server

```
server <- function(input, output) {
  # Reactive expression to load data
  data <- reactive({
    rds_path <- "~/Documents/DATA/Data 332/files given/Uber data"
    uber_data_files <- paste0("uber-raw-data-", c("apr14", "may14", "jun14", "jul14", "aug14", "sep14"), ".csv")
    full_paths <- file.path(rds_path, uber_data_files)
```
Hourly Trips: Visualizes the number of trips by hour.
```
 output$hourly_trips_month_plot <- renderPlot({
    hourly_trips_month <- data() %>%
      mutate(Hour = format(`Date/Time`, "%H"),
             Month = format(`Date/Time`, "%m")) %>%
```
Hourly Trips by Month: Displays a line chart of trips by hour and month.
```
hourly_trips_month <- combined_data %>%
    mutate(Hour = format(Date.Time, format = "%H"),
           Month = format(Date.Time, format = "%m")) %>%
   
```

Trips Every Hour: Shows the trend of trips taken every hour.
```
  hourly_trips <- combined_data %>%
    group_by(Date.Time) %>%
    summarize(Trips = n())
```



Trips Every Day of the Month: Visualizes the number of trips taken each day of the month.
```
daily_monthly_trips <- combined_data %>%
    mutate(Date = as.Date(Date.Time),  # Extract date from Date.Time
           Month = format(Date.Time, "%m")) %>%  # Extract month from Date.Time

```



Trips by Day and Month: Displays a bar chart of trips by day and month.
```
trips_by_day_month <- daily_monthly_trips %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Weekday = weekdays(Date)) %>%
    
  ```



Trips by Bases and Month: Shows the distribution of trips by bases and month.
```
 trips_by_bases_month <- ggplot(trips_by_base, aes(x = Base, y = Total, fill = factor(Base))) +      
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Trips by Bases and Month", x = "Base", y = "Number of Trips", fill = "Base") +
    theme_minimal()
```


Heatmap of Uber Trips by Hour and Day: Presents a heatmap of trips by hour and day.
```
 day_and_hour <- combined_data %>%
    mutate(Day_of_Week = factor(weekdays(Date.Time), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    mutate(Hour = format(Date.Time, "%H")) %>%
    group_by(Day_of_Week, Hour) %>%
    summarize(Trips = n())
```



Heatmap of Uber Trips by Month and Day: Displays a heatmap of trips by month and day.
```
day_month_group <- combined_data %>%
    mutate(Month = format(Date.Time, "%m"),
           Day = format(Date.Time, "%d")) %>%
    group_by(Month, Day) %>%
    summarize(Trips = n())
```


  
Heatmap of Uber Trips by Month and Week: Shows a heatmap of trips by month and week.
```
 month_week <- combined_data %>%
    mutate(Week = format(Date.Time, "%U"), # Added comma here
           Month = format(Date.Time, "%m")) %>%
    group_by(Month, Week) %>%
    summarize(Trips = n())
```
 

  
Heatmap of Uber Trips by Base and Day of Week: Presents a heatmap of trips by base and day of week.
```
 base_day_group <- combined_data %>%
    mutate(Day_of_Week = factor(weekdays(Date.Time), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    group_by(Base, Day_of_Week) %>%
    summarize(Trips = n())
```
  <img src="Images/4.3 month & week.png" height = 250, width = 400>

  
Trips byBase and day of the week:
```
 base_day_group <- combined_data %>%
    mutate(Day_of_Week = factor(weekdays(Date.Time), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    group_by(Base, Day_of_Week) %>%
    summarize(Trips = n())
```
 <img src="Images/4.4 day of week.png" height = 250, width = 400>


# Geospatial leaflet map
- Leaflet Map: Displays a geospatial heatmap of Uber trips using Leaflet.
-  Initialize leaflet map
-  ```
   heatmap_data <- combined_data %>%
    group_by(Lat, Lon) %>%
    summarize(Count = n()) %>%
    filter(!is.na(Lat) & !is.na(Lon))
   ```
   Creating the Leaflet map with heatmap
```
 output$leaflet_map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addHeatmap(
        data = heatmap_data, # Changed combined_data to heatmap_data
        lng = ~Lon,
        lat = ~Lat,
        intensity = ~Count,
        radius = 20
      ) %>% 
      addLegend(
        position = "bottomright",
        pal = colorNumeric(palette = "viridis", domain = heatmap_data$Count),
        values = heatmap_data$Count,
        title = "Number of Trips"
      )
  })
}
```

## Run the app
```
shinyApp(ui, server)
```
