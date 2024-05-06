
library(sparklyr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(leaflet)
library(leaflet.extras)
library(shiny)

# UI part
ui <- fluidPage(
  
  titlePanel("Uber Trips Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # You can add any input widgets here if needed
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Hourly Trips", plotOutput("hourly_trips_plot")),
        tabPanel("Hourly Trips by Month", plotOutput("hourly_trips_month_plot")),
        tabPanel("Trips Every Hour", plotOutput("hourly_trips_every_hour_plot")),
        tabPanel("Trips Every Day of the Month", plotOutput("daily_trips_month_plot")),
        tabPanel("Trips by Day and Month", plotOutput("trips_by_day_month_plot")),
        tabPanel("Trips by Bases and Month", plotOutput("trips_by_bases_month_plot")),
        tabPanel("Heatmap of Uber Trips by Hour and Day", plotOutput("heatmap_hour_day")),
        tabPanel("Heatmap of Uber Trips by Month and Day", plotOutput("heatmap_month_day")),
        tabPanel("Heatmap of Uber Trips by Month and Week", plotOutput("heatmap_month_week")),
        tabPanel("Heatmap of Uber Trips by Base and Day of Week", plotOutput("heatmap_base_day")),
        tabPanel("Leaflet Map", leafletOutput("leaflet_map"))
      )
    )
  )
)

# Server part
server <- function(input, output) {
  
  
  # Step 1:
  data04 <- read.csv("uber-raw-data-apr14.csv")
  data05 <- read.csv("uber-raw-data-apr14.csv")
  data06 <- read.csv("uber-raw-data-apr14.csv")
  data07 <- read.csv("uber-raw-data-apr14.csv")
  data08 <- read.csv("uber-raw-data-apr14.csv")
  data09 <- read.csv("uber-raw-data-apr14.csv")
  #Step 1: Bind all the data together
  combined_data <- bind_rows(data04, data05, data05, data07, data08, data09)
  
  #Step 2: Changing the date column to a date schema
  combined_data$Date.Time <- as.POSIXct(combined_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
  
  # Step 3.1: Pivot table to display trips by the hour.
  hourly_trips <- combined_data %>%
    mutate(Hour = format(Date.Time, format = "%H")) %>%
    group_by(Hour) %>%
    summarize(Trips = n())
  print(hourly_trips)
  
  # 3.2: Chart that shows Trips by Hour and Month
  
  hourly_trips_month <- combined_data %>%
    mutate(Hour = format(Date.Time, format = "%H"),
           Month = format(Date.Time, format = "%m")) %>%
    group_by(Month, Hour) %>%
    summarize(Trips = n()) %>%
    ggplot(aes(x = Hour, y = Trips, color = Month)) +
    geom_line() +
    labs(title = "Trips by Hour and Month")
  # Print the chart
  print(hourly_trips_month)
  
  # 3.3 Chart that displays Trips Every Hour.
  hourly_trips <- combined_data %>%
    group_by(Date.Time) %>%
    summarize(Trips = n())
  
  #  plot
  ggplot(hourly_trips, aes(x = Date.Time, y = Trips)) +
    geom_line() +
    labs(x = "Hour of Day", y = "Trips") +
    ggtitle("Trips Every Hour")
  
  
  #3.4, Plot data by trips taken during every day of the month.
  daily_monthly_trips <- combined_data %>%
    mutate(Date = as.Date(Date.Time),  # Extract date from Date.Time
           Month = format(Date.Time, "%m")) %>%  # Extract month from Date.Time
    group_by(Date, Month) %>%
    summarize(Trips = n())
  #plot
  ggplot(daily_monthly_trips, aes(x = Date, y = Trips)) +
    geom_line() +
    labs(x = "Date", y = "Trips", title = "Trips Every Day of the Month") +
    theme_minimal()
  
  #3.5, table that shows Trips Every Day (Max 31 days in a month so I should see total trips taken each day). 
  daily_trips_table <- daily_monthly_trips %>%
    group_by(Date) %>%
    summarize(Trips = sum(Trips)) %>%
    kable()
  print(daily_trips_table)
  
  #3.6, Chart by Trips by Day and Month (bar chart with each day of the week, x axis as the month). I need a chart that shows number of trips by month
  trips_by_day_month <- daily_monthly_trips %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Weekday = weekdays(Date)) %>%
    ggplot(aes(x = Month, y = Trips, fill = Weekday)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Trips by Day and Month", x = "Month", y = "Number of Trips", fill = "Day of Week") +
    theme_minimal()
  print(trips_by_day_month)
  
  #3.7, Chart Trips by Bases and Month (Base is the X axis and Month is your label)
  # Chart showing trips by bases and month
  trips_by_base <- combined_data %>%
    group_by(Base) %>%
    summarize(Total = n())
  
  # Plot trips by bases and month                                                                   ????????
  trips_by_bases_month <- ggplot(trips_by_base, aes(x = Base, y = Total, fill = factor(Base))) +      
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Trips by Bases and Month", x = "Base", y = "Number of Trips", fill = "Base") +
    theme_minimal()
  print(trips_by_bases_month)
  
  # Step 4.1: Group the data by hour and day
  day_and_hour <- combined_data %>%
    mutate(Day_of_Week = factor(weekdays(Date.Time), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    mutate(Hour = format(Date.Time, "%H")) %>%
    group_by(Day_of_Week, Hour) %>%
    summarize(Trips = n())
  # Plot the heatmap
  ggplot(day_and_hour, aes(x = Hour, y = Day_of_Week, fill = Trips)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "pink", high = "steelblue") +
    labs(x = "Hour of Day", y = "Day of Week", title = "Heatmap of Uber Trips by Hour and Day")
  
  #4.2, Heat map by month and day
  day_month_group <- combined_data %>%
    mutate(Month = format(Date.Time, "%m"),
           Day = format(Date.Time, "%d")) %>%
    group_by(Month, Day) %>%
    summarize(Trips = n())
  
  # Plot the heatmap for trips by month and day
  ggplot(day_month_group, aes(x = Day, y = Month, fill = Trips)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colors = c("pink", "steelblue"), na.value = "grey90") +
    labs(x = "Day of Month", y = "Month", title = "Heatmap of Uber Trips by Month and Day")
  
  #Heat map by month and week
  # Step 4.3: Group the data by month and week
  month_week <- combined_data %>%
    mutate(Week = format(Date.Time, "%U"), # Added comma here
           Month = format(Date.Time, "%m")) %>%
    group_by(Month, Week) %>%
    summarize(Trips = n())
  # Plot the heatmap
  ggplot(month_week, aes(x = Week, y = Month, fill = Trips)) +
    geom_tile(color = "white") +
    ggtitle("Heatmap of Uber Trips by Month and Week") +
    scale_fill_gradientn(colors = c("pink", "steelblue"), na.value = "grey90") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_text(color = "black", size = 12, face = "bold", margin = margin(t = 10)),
          axis.title.y = element_text(color = "black", size = 12, face = "bold", margin = margin(r = 10)))
  
  
  #Heat map Bases and Day of Week
  base_day_group <- combined_data %>%
    mutate(Day_of_Week = factor(weekdays(Date.Time), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    group_by(Base, Day_of_Week) %>%
    summarize(Trips = n())
  
  # Plot the heatmap for trips by bases and day of week
  ggplot(base_day_group, aes(x = Day_of_Week, y = Base, fill = Trips)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "pink", high = "steelblue") +
    labs(x = "Day of Week", y = "Base", title = "Heatmap of Uber Trips by Base and Day of Week")
  
  #5 Leaflet Shiny Geospatial Map 
  # Prepare heatmap data
  heatmap_data <- combined_data %>%
    group_by(Lat, Lon) %>%
    summarize(Count = n()) %>%
    filter(!is.na(Lat) & !is.na(Lon))
  
  # Create Leaflet map with heatmap
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

# Run the Shiny app
shinyApp(ui = ui, server = server)

