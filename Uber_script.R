library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(tidyr)
library(lubridate)

# Define the UI
ui <- fluidPage(
  titlePanel("Uber Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      checkboxGroupInput("dayOfWeekSelector", "Select Days of the Week:",
                         choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                         selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
      selectInput("monthSelector", "Select Month:",
                  choices = c("All", "April", "May", "June", "July", "August", "September"),
                  selected = "All"),
      p("")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualizations", fluid = TRUE,
                 plotOutput("hourly_trips_month_plot"),
                 plotOutput("hourly_trips_every_hour_plot"),
                 plotOutput("daily_trips_month_plot"),
                 plotOutput("trips_by_day_month_plot"),
                 plotOutput("trips_by_bases_month_plot"),
                 plotOutput("heatmap_hour_day"),
                 plotOutput("heatmap_month_day"),
                 plotOutput("heatmap_month_week"),
                 plotOutput("heatmap_base_day")
                 
        ),
        tabPanel("Data Tables",
                 tableOutput("trips_every_day")
                 
        ),
        tabPanel("Geospatial Map",
                 leafletOutput("leaflet_map")
        )
      )
    )
  )
)

# Server part
server <- function(input, output) {
  # Reactive expression to load data
  data <- reactive({
    rds_path <- "~/Documents/DATA/Data 332/files given/Uber data"
    uber_data_files <- paste0("uber-raw-data-", c("apr14", "may14", "jun14", "jul14", "aug14", "sep14"), ".csv")
    full_paths <- file.path(rds_path, uber_data_files)
    
    if(all(file.exists(full_paths))) {
      combined_data <- lapply(full_paths, read_csv) %>%
        bind_rows() %>%
        mutate(`Date/Time` = as.POSIXct(`Date/Time`, format = "%m/%d/%Y %H:%M:%S"))
      return(combined_data)
    } else {
      stop("One or more files do not exist in the specified path.")
    }
  })
  
  # Chart that shows Trips by Hour and Month
  output$hourly_trips_month_plot <- renderPlot({
    hourly_trips_month <- data() %>%
      mutate(Hour = format(`Date/Time`, "%H"),
             Month = format(`Date/Time`, "%m")) %>%
      group_by(Month, Hour) %>%
      summarize(Trips = n()) %>%
      ungroup()
    ggplot(hourly_trips_month, aes(x = Hour, y = Trips, color = Month)) +
      geom_line() +
      labs(title = "Trips by Hour and Month", x = "Hour", y = "Trips")
  })
  
  # Chart that displays Trips Every Hour
  output$hourly_trips_every_hour_plot <- renderPlot({
    hourly_trips <- data() %>%
      group_by(`Date/Time`) %>%
      summarize(Trips = n()) %>%
      ungroup()
    ggplot(hourly_trips, aes(x = `Date/Time`, y = Trips)) +
      geom_line() +
      labs(title = "Trips Every Hour", x = "Hour of Day", y = "Trips")
  })
  
  # Plot data by trips taken during every day of the month
  output$daily_trips_month_plot <- renderPlot({
    daily_monthly_trips <- data() %>%
      mutate(Date = as.Date(`Date/Time`)) %>%
      group_by(Date) %>%
      summarize(Trips = n()) %>%
      ungroup()
    ggplot(daily_monthly_trips, aes(x = Date, y = Trips)) +
      geom_line() +
      labs(title = "Trips Every Day of the Month", x = "Date", y = "Trips")
  })
  
  # Table that shows Trips Every Day
  output$trips_every_day <- renderTable({
    daily_trips_table <- data() %>%
      mutate(Date = as.Date(`Date/Time`)) %>%
      group_by(Date) %>%
      summarize(Trips = n())  # Counting the number of trips per day
    daily_trips_table
  })
  
  # Chart by Trips by Day and Month
  output$trips_by_day_month_plot <- renderPlot({
    trips_by_day_month <- data() %>%
      mutate(Date = as.Date(`Date/Time`),
             Weekday = weekdays(Date),
             Month = format(`Date/Time`, "%m")) %>%
      group_by(Month, Weekday) %>%
      summarize(Trips = n()) %>%
      ungroup() 
    ggplot(trips_by_day_month, aes(x = Month, y = Trips, fill = Weekday)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Day and Month", x = "Month", y = "Number of Trips", fill = "Day of Week") +
      theme_minimal()
  })
  
  # Chart Trips by Bases and Month
  output$trips_by_bases_month_plot <- renderPlot({
    trips_by_base <- data() %>%
      mutate(Month = format(`Date/Time`, "%m")) %>%
      group_by(Base, Month) %>%
      summarize(Trips = n()) %>%
      ungroup() 
    ggplot(trips_by_base, aes(x = Base, y = Trips, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Bases and Month", x = "Base", y = "Number of Trips", fill = "Month") +
      theme_minimal()
  })
  
  # Heatmap of Uber Trips by Hour and Day
  output$heatmap_hour_day <- renderPlot({
    day_and_hour <- data() %>%
      mutate(Day_of_Week = factor(weekdays(`Date/Time`), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
             Hour = format(`Date/Time`, "%H")) %>%
      group_by(Day_of_Week, Hour) %>%
      summarize(Trips = n())
    ggplot(day_and_hour, aes(x = Hour, y = Day_of_Week, fill = Trips)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "pink", high = "steelblue") +
      labs(x = "Hour of Day", y = "Day of Week", title = "Heatmap of Uber Trips by Hour and Day")
  })
  
  # Heatmap of Uber Trips by Month and Day
  output$heatmap_month_day <- renderPlot({
    day_month_group <- data() %>%
      mutate(Month = format(`Date/Time`, "%m"),
             Day = format(`Date/Time`, "%d")) %>%
      group_by(Month, Day) %>%
      summarize(Trips = n())
    ggplot(day_month_group, aes(x = Day, y = Month, fill = Trips)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = c("pink", "steelblue"), na.value = "grey90") +
      labs(x = "Day of Month", y = "Month", title = "Heatmap of Uber Trips by Month and Day")
  })
  
  # Heatmap of Uber Trips by Month and Week
  output$heatmap_month_week <- renderPlot({
    month_week <- data() %>%
      mutate(Week = format(`Date/Time`, "%U"),
             Month = format(`Date/Time`, "%m")) %>%
      group_by(Month, Week) %>%
      summarize(Trips = n())
    ggplot(month_week, aes(x = Week, y = Month, fill = Trips)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = c("pink", "steelblue"), na.value = "grey90") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_text(color = "black", size = 12, face = "bold", margin = margin(t = 10)),
            axis.title.y = element_text(color = "black", size = 12, face = "bold", margin = margin(r = 10)))
  })
  
  # Heatmap of Uber Trips by Base and Day of Week
  output$heatmap_base_day <- renderPlot({
    base_day_group <- data() %>%
      mutate(Day_of_Week = factor(weekdays(`Date/Time`), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      group_by(Base, Day_of_Week) %>%
      summarize(Trips = n())
    ggplot(base_day_group, aes(x = Day_of_Week, y = Base, fill = Trips)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "pink", high = "steelblue") +
      labs(x = "Day of Week", y = "Base", title = "Heatmap of Uber Trips by Base and Day of Week")
  })
  
  # Leaflet Shiny Geospatial Map
  output$leaflet_map <- renderLeaflet({
    heatmap_data <- data() %>%
      group_by(Lat, Lon) %>%
      summarize(Count = n()) %>%
      filter(!is.na(Lat) & !is.na(Lon))
    leaflet(heatmap_data) %>%
      addTiles() %>%
      addHeatmap(lng = ~Lon, lat = ~Lat, intensity = ~Count, radius = 20) %>%
      addLegend(position = "bottomright", pal = colorNumeric(palette = "viridis", domain = heatmap_data$Count), values = heatmap_data$Count, title = "Number of Trips")
  })
  # Analyze common days with higher trips
  output$common_days_high_trips_plot <- renderPlot({
    high_trip_days <- data() %>%
      mutate(Date = as.Date(`Date/Time`)) %>%
      group_by(Date) %>%
      summarize(Trips = n()) %>%
      ungroup() %>%
      arrange(desc(Trips)) %>%
      top_n(10, Trips)  # Top 10 days with the highest number of trips
    ggplot(high_trip_days, aes(x = reorder(Date, Trips), y = Trips, fill = Trips)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Top 10 Days with Highest Trips", x = "Date", y = "Number of Trips") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$common_days_high_trips_table <- renderTable({
    high_trip_days <- data() %>%
      mutate(Date = as.Date(`Date/Time`)) %>%
      group_by(Date) %>%
      summarize(Trips = n()) %>%
      ungroup() %>%
      arrange(desc(Trips)) %>%
      top_n(10, Trips)  # Top 10 days with the highest number of trips
    high_trip_days
  })
}

# Run the Shiny app
shinyApp(ui, server)
