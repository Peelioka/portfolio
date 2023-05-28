install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(lubridate)
library(hms)

## The libraries I'm expecting to use

## I'm using this to clear the environment for each run cycle.
rm(list = ls())

## cdx for each csv of cyclistic data by month
cd1 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202201-divvy-tripdata.csv")
cd2 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202202-divvy-tripdata.csv")
cd3 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202203-divvy-tripdata.csv")
cd4 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202204-divvy-tripdata.csv")
cd5 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202205-divvy-tripdata.csv")
cd6 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202206-divvy-tripdata.csv")
cd7 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202207-divvy-tripdata.csv")
cd8 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202208-divvy-tripdata.csv")
cd9 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202209-divvy-tripdata.csv")
cd10 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202210-divvy-tripdata.csv")
cd11 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202211-divvy-tripdata.csv")
cd12 <- read.csv("C:\\Users\\Oli-p\\Documents\\R\\CyclisticData\\RawData\\202212-divvy-tripdata.csv")

## Combine all the individual data frames into a single data frame.
cycle_data <- rbind(cd1,cd2,cd3,cd4,cd5,cd6,cd7,cd8,cd9,cd10,cd11,cd12)

head(cycle_data)
str(cycle_data)

## The structure function is showing that the 'started_at' and 'ended_at' columns have been imported as characters
## Using lubridate to convert to a date format. (Note, changes from dmy in Excel to ymd in R when CSV imported)

cycle_data$started_at <- lubridate::ymd_hms(cycle_data$started_at)
cycle_data$ended_at <- lubridate::ymd_hms(cycle_data$ended_at)

## Now that it's in a datetime format I'll use Lubridate again to split the date and the time fields.
## This will create new fields, Date and Time

cycle_data$start_time <- format(as.POSIXct(cycle_data$started_at),format = "%H:%M:%S")
cycle_data$end_time <- format(as.POSIXct(cycle_data$ended_at),format = "%H:%M:%S")

cycle_data$start_date <- lubridate::as_date(cycle_data$started_at)
cycle_data$end_date <- lubridate::as_date(cycle_data$ended_at)

## Now creating a day of the week field to go with the date field
cycle_data$start_weekday <- weekdays(cycle_data$start_date)
cycle_data$end_weekday <- weekdays(cycle_data$end_date)

## Creating an hour field for a departure time graph
cycle_data$hour_departed <- hour(cycle_data$started_at)

## Creating a month field for a month memb/casual ratio graph.
## As the lubridate month function creates it in a numerical format I then convert it to text.
cycle_data$month_of_ride <- month(cycle_data$started_at)
cycle_data$month_of_ride_txt <- month.abb[cycle_data$month_of_ride]

## Checking the row count for the function below
dim(cycle_data)


## looking into the CSV files there are some empty entries places.

## These functions should remove any completely empty rows and columns
cycle_data <- janitor::remove_empty(cycle_data, which = c("cols"))
cycle_data <- janitor::remove_empty(cycle_data, which = c("rows"))

## rechecking the row count to see if anything is removed.
dim(cycle_data)

## This function will remove any rows with N/a entries.
cycle_data <- cycle_data %>% drop_na()

## rechecking the row count to see if anything is removed.
dim(cycle_data)

## 5858 N/a rows removed.

## Processing the time data to get the ride time data points, shows how long rides lasted.
cycle_data$ride_duration <- as_hms(difftime(cycle_data$ended_at,cycle_data$started_at))

## Next step is the process the latitude and longitude movements to get a distance travelled.

## From research it looks like the Haversine formula is needed for the calcs.
## First step is to convert the longitudinal and latitudinal points into radians.
cycle_data$start_lat_rads <- cycle_data$start_lat/(180/pi)
cycle_data$start_lng_rads <- cycle_data$start_lng/(180/pi)
cycle_data$end_lat_rads <- cycle_data$end_lat/(180/pi)
cycle_data$end_lng_rads <- cycle_data$end_lng/(180/pi)

## A few more variables that will be needed
radius <- 6378.8
cycle_data$dlat <- (cycle_data$start_lat_rads-cycle_data$end_lat_rads)
cycle_data$dlong <- (cycle_data$start_lng_rads-cycle_data$end_lng_rads)

## Now to apply the Haversine formula.
## Some of the results were coming out as N/a where the cyclist returns to their starting location.
## The ifelse statement checks for examples where cyclists returned to the start point. This removes the N/a results.

cycle_data$distance_travelled_km <- ifelse(cycle_data$dlat == 0 & cycle_data$dlong == 0, 0, 
                                           cycle_data$distance_travelled_km <- radius*acos((sin(cycle_data$start_lat_rads)*sin(cycle_data$end_lat_rads))+(cos(cycle_data$start_lat_rads)*cos(cycle_data$end_lat_rads)*cos(cycle_data$dlong))))

## Data needed for the user split pie chart.

user_total <- nrow(cycle_data)
member_total <- nrow(cycle_data[cycle_data$member_casual == 'member',])
casual_total <- nrow(cycle_data[cycle_data$member_casual == 'casual',])
member_percent <- (member_total/user_total)
casual_percent <- (casual_total/user_total)
pie_user <- c("Member","Casual")
pie_total <- c(member_total,casual_total)
pie_percent <- c(member_percent,casual_percent)
pie_df <- data.frame(pie_user,pie_total,pie_percent)

## Some final clean up.
dim(cycle_data)

## The rideable_type contains three types, "classic bike", "docked bike" and "electric bike"
## The docked bike entries need to be removed as they are quality control tests by cyclistic.
cycle_data <- cycle_data[!(cycle_data$rideable_type == "docked_bike"),]

dim(cycle_data)

## Removing the station name and station id columns. 
## Some entries in them are blank and all the info needed has been gained from the latidude and longitude.
cycle_data <- cycle_data %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))

dim(cycle_data)

## Some ride durations are coming back as negative times. These are clearly errors and need to be removed to avoid skewing the data.
## Any rides of less than 1 minute are also removed as they are likely started in error.
cycle_data <- cycle_data[!(cycle_data$ride_duration<=60),]

dim(cycle_data)

## Ride distances greater than 50km are removed as they are impossible within Chicago; therefore the data is incorrect
cycle_data <- cycle_data[!(cycle_data$distance_travelled_km>=50),]

## rechecking the row count to see if anything is removed.
dim(cycle_data)



## bar chart of premium vs casual member rides on different week days
plot_days <- ggplot(data=cycle_data)+
              geom_bar(mapping = aes(x=start_weekday,fill = member_casual),position = "dodge")+
                scale_y_continuous(labels = scales::comma, 
                                   breaks = c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000), 
                                   minor_breaks = NULL)+
                scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))+
                labs(title = "Members vs Casuals - Day of the Week", fill = "Type of User")+
                xlab("Days of Journey") + ylab("Total Rides")+
                theme(plot.title = element_text(size=15, face="bold.italic"), 
                      axis.title.x = element_text(size=13, face="bold", vjust = -1.25), 
                      axis.title.y = element_text(size=13, face="bold", vjust = 3),
                      legend.title = element_text(size=11, face="bold"),
                      axis.text.x = element_text(face = "bold"),
                      axis.text.y = element_text(face = "bold"),
                      legend.text = element_text(face = "bold"))+
                scale_fill_discrete(labels = c("Casual", "Member"))



## bar chart of premium vs casual members for the time they start a journey.
plot_time_of_day <- ggplot(data=cycle_data)+
                      geom_bar(mapping = aes(x=hour_departed,fill = member_casual),position = "dodge")+
                        scale_y_continuous(labels = scales::comma, 
                                           breaks = c(0,50000,100000,150000,200000,250000,300000,350000),
                                           minor_breaks = NULL)+
                        scale_x_continuous(breaks = c(0:23),
                                           minor_breaks = NULL)+
                        labs(title = "Members vs Casuals - Time Users Start Jouneys", fill = "Type of User")+
                        xlab("The Hour Journeys Begin") + ylab("Total Rides")+
                        theme(plot.title = element_text(size=15, face="bold.italic"), 
                              axis.title.x = element_text(size=13, face="bold", vjust = -1.25), 
                              axis.title.y = element_text(size=13, face="bold", vjust = 3),
                              legend.title = element_text(size=11, face="bold"),
                              axis.text.x = element_text(face = "bold"),
                              axis.text.y = element_text(face = "bold"),
                              legend.text = element_text(face = "bold"))+
                        scale_fill_discrete(labels = c("Casual", "Member"))

## bar chart of premium vs casual members for the months they ride.
plot_months <- ggplot(data=cycle_data)+
                geom_bar(mapping = aes(x=month_of_ride_txt,fill = member_casual),position = "dodge")+
                  scale_y_continuous(labels = scales::comma, 
                                     breaks = c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000), 
                                     minor_breaks = NULL)+
                  scale_x_discrete(limits = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
                  labs(title = "Members vs Casuals - Time of Year Users Ride", fill = "Type of User")+
                  xlab("Month of Journey") + ylab("Total Rides")+
                  theme(plot.title = element_text(size=15, face="bold.italic"), 
                        axis.title.x = element_text(size=13, face="bold", vjust = -1.25), 
                        axis.title.y = element_text(size=13, face="bold", vjust = 3),
                        legend.title = element_text(size=11, face="bold"),
                        axis.text.x = element_text(face = "bold"),
                        axis.text.y = element_text(face = "bold"),
                        legend.text = element_text(face = "bold"))+
                  scale_fill_discrete(labels = c("Casual", "Member"))

## bar chart of different bike types cycled by members vs casuals.

plot_bike_type <- ggplot(data=cycle_data)+
                    geom_bar(mapping = aes(x=rideable_type,fill = member_casual),position = "dodge")+
                      scale_y_continuous(labels = scales::comma,
                                         breaks = c(0,250000,500000,750000,1000000,1250000,1500000,1750000))+
                      labs(title = "Members vs Casual - Day of the Week", fill = "Type of User")+
                      xlab("Type of Bike") + ylab("Total Rides")+
                      theme(plot.title = element_text(size=15, face="bold.italic"), 
                            axis.title.x = element_text(size=13, face="bold", vjust = -1.25), 
                            axis.title.y = element_text(size=13, face="bold", vjust = 3),
                            legend.title = element_text(size=11, face="bold"),
                            axis.text.x = element_text(face = "bold"),
                            axis.text.y = element_text(face = "bold"),
                            legend.text = element_text(face = "bold"))+
                      scale_fill_discrete(labels = c("Casual", "Member"))+
                      scale_x_discrete(labels = c("Classic Bike", "Electric Bike"))
                      

## Pie Chart of total member vs casual journeys

plot_user_pie <- ggplot(data=pie_df, aes(x = "", y = pie_percent, fill = pie_user))+
                    geom_bar(stat = "identity", colour="black")+
                    coord_polar("y")+
                    geom_text(aes(label = paste0(round(pie_percent*100,0), "%")), position = position_stack(vjust = 0.5))+
                    labs(title = "Members vs Casuals - Total Journey's", fill = "Type of User")+
                    theme_void()+
                    theme(plot.title = element_text(size=15, face="bold.italic"),
                          legend.title = element_text(size=11, face="bold"),
                          legend.text = element_text(face = "bold"))+
                    scale_fill_discrete(labels = c("Casual", "Member"))

## Line Graph of distance against date

plot_distance_travelled <- ggplot(data=cycle_data)+
                            geom_smooth(mapping = aes(x=start_date, y=distance_travelled_km, colour = member_casual))+
                              labs(title = "Members vs Casuals - Average Distance Travelled", colour = "Type of User")+
                              xlab("Date of Journey") + ylab("Distance Travelled - Km")+
                              theme(plot.title = element_text(size=15, face="bold.italic"), 
                                    axis.title.x = element_text(size=13, face="bold", vjust = -1.25), 
                                    axis.title.y = element_text(size=13, face="bold", vjust = 3),
                                    legend.title = element_text(size=11, face="bold"),
                                    axis.text.x = element_text(face = "bold"),
                                    axis.text.y = element_text(face = "bold"),
                                    legend.text = element_text(face = "bold"))+
                              scale_color_discrete(labels = c("Casual", "Member"))+
                              expand_limits(y=0)+
                              scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8),
                                                 minor_breaks = NULL)+
                              scale_x_date(breaks = "1 month", 
                                           minor_breaks = NULL, 
                                           date_labels = "%B")

  
## Line Graph of time spent cycling against date

plot_time_cycled <- ggplot(data=cycle_data)+
                      geom_smooth(mapping = aes(x=start_date,y=ride_duration, colour = member_casual))+
                        labs(title = "Members vs Casuals - Average Ride Duration", fill = "Type of User", colour = "Type of User")+
                        xlab("Date of Journey") + ylab("Ride Duration")+
                        theme(plot.title = element_text(size=15, face="bold.italic"), 
                              axis.title.x = element_text(size=13, face="bold", vjust = -1.25), 
                              axis.title.y = element_text(size=13, face="bold", vjust = 3),
                              legend.title = element_text(size=11, face="bold"),
                              axis.text.x = element_text(face = "bold"),
                              axis.text.y = element_text(face = "bold"),
                              legend.text = element_text(face = "bold"))+
                        scale_color_discrete(labels = c("Casual", "Member"))+
                        expand_limits(y=0)+
                        scale_y_time(breaks = scales::breaks_width("2 min"),
                                     minor_breaks = NULL)+
                        scale_x_date(breaks = "1 month", 
                                      minor_breaks = NULL, 
                                      date_labels = "%B")

plot_days
plot_time_of_day
plot_months
plot_bike_type
plot_user_pie
plot_distance_travelled
plot_time_cycled